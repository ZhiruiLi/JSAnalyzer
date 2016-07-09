module JSParser
( jsProgramParser
, jsValueParser
, jsExpressionParser
, jsStatementParser
, parseMaybe
, parseTest
, parseString
) where

import           Control.Monad              (void)
import           Data.List                  (splitAt)
import           Debug.Trace
import           JSEntity
import           JSError                    (JSError (ParserError), ThrowsError,
                                             throwError)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Combinator
import qualified Text.Megaparsec.Expr       as E
import qualified Text.Megaparsec.Lexer      as L
import           Text.Megaparsec.Pos
import           Text.Megaparsec.String

-- | words that can't be used as identifiers
reservedWords = ["break", "do", "instanceof", "typeof", "case", "else", "new",
  "var", "catch", "finally", "return", "void", "continue", "for", "switch",
  "while", "debugger", "function", "this", "with", "default", "if", "throw",
  "delete", "in", "try", "class", "enum", "extends", "super", "const", "export",
  "import", "implements", "let", "private", "public", "yield", "interface",
  "package", "protected", "static"
  ]

-- | parse a JS integer represent by decimal or hexadecimal
--
-- Examples:
-- >>> parseMaybe pJSInt "123"
-- Just 123
-- >>> parseMaybe pJSInt "0xf"
-- Just 15
-- >>> parseMaybe pJSInt "0123"
-- Just 123
-- >>> parseMaybe pJSInt "123a"
-- Nothing
-- >>> parseMaybe pJSInt "0xfg"
-- Nothing
pJSInt :: Parser JSVal
pJSInt  = JSInt <$> (try hexadecimal <|> L.decimal)
  where
    hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

-- | parse a JS floating point number
--
-- Examples:
-- >>> parseMaybe pJSFloat "1.2"
-- Just 1.2
-- >>> parseMaybe pJSFloat "0.12"
-- Just 0.12
-- >>> parseMaybe pJSFloat "12"
-- Nothing
pJSFloat :: Parser JSVal
pJSFloat  = JSFloat <$> L.float

-- | parse a JS string, a JS string can be quoted by " or '
--
-- Examples:
-- >>> parseMaybe pJSString "\" abc''\""
-- Just  abc''
-- >>> parseMaybe pJSString "' abc '"
-- Just  abc
-- >>> parseMaybe pJSString "' abc \"\\\\'"
-- Just  abc "\
-- >>> parseMaybe pJSString "'\\n'"
-- Just
-- <BLANKLINE>
-- >>> parseMaybe pJSString "''"
-- Just
pJSString :: Parser JSVal
pJSString =  JSString <$> (try doubleQuoteString <|> singleQuoteString)
  where
    doubleEscapeChar :: Parser Char
    doubleEscapeChar  = pEscapeChar "nbfrtv\\\""
    singleEscapeChar :: Parser Char
    singleEscapeChar  = pEscapeChar "nbfrtv\\'"
    doubleNormalChar :: Parser Char
    doubleNormalChar  = noneOf "\r\n\\\""
    singleNormalChar :: Parser Char
    singleNormalChar  = noneOf "\r\n\\'"
    quoted  :: String -> Parser a -> Parser a
    quoted q = between (string q) (string q)
    doubleQuoteString :: Parser String
    doubleQuoteString  = quoted "\""
      (many (doubleEscapeChar <|> doubleNormalChar))
    singleQuoteString :: Parser String
    singleQuoteString  = quoted "'"
      (many (singleEscapeChar <|> singleNormalChar))

-- | parse a escape char which is a char after '\', for 'n' 'b' 'f' 'r' 't' 'v',
-- it will convert it to '\n' '\b' '\f' '\r' '\t' '\v', for other chars, it will
-- leave it as it is, all chars need to be parsed should be declared in the
-- input string
--
-- Examples:
-- >>> parseMaybe (pEscapeChar "n") "\\n"
-- Just '\n'
-- >>> parseMaybe (pEscapeChar "r") "\\n"
-- Nothing
-- >>> parseMaybe (pEscapeChar "xyz") "\\x"
-- Just 'x'
pEscapeChar       :: String -> Parser Char
pEscapeChar chars  = do c <- char '\\' >> oneOf chars
                        return $ case c of
                                   'n' -> '\n'
                                   'b' -> '\b'
                                   'f' -> '\f'
                                   'r' -> '\r'
                                   't' -> '\t'
                                   'v' -> '\v'
                                   _   -> c

-- | parse a JS boolean value, representing True as true, False as false
--
-- Examples:
-- >>> parseTest pJSBool "true "
-- true
-- >>> parseTest pJSBool "false "
-- false
-- >>> parseMaybe pJSBool "true "
-- Nothing
pJSBool :: Parser JSVal
pJSBool  = JSBool <$>
  ((string "true" >> return True) <|> (string "false" >> return False))

-- | parse JS null value, representing as null
--
-- Examples:
-- >>> parseMaybe pJSNull "null"
-- Just null
-- >>> parseMaybe pJSNull "null "
-- Nothing
pJSNull :: Parser JSVal
pJSNull  = string "null" >> return JSNull

-- | parse JS undefined value, representing as undefined
--
-- Examples:
-- >>> parseMaybe pJSUndefined "undefined"
-- Just undefined
-- >>> parseMaybe pJSUndefined "undefined "
-- Nothing
pJSUndefined :: Parser JSVal
pJSUndefined  = string "undefined" >> return JSUndefined

-- | parse JS this object, representing as this
--
-- Examples:
-- >>> parseMaybe pJSThis "this"
-- Just this
-- >>> parseMaybe pJSThis "this "
-- Nothing
pJSThis :: Parser JSVal
pJSThis  = string "this" >> return JSThis

-- | parse JS NaN literial
--
-- Examples:
-- >>> parseMaybe pJSNaN "NaN"
-- Just NaN
-- >>> parseMaybe pJSNaN "NaN "
-- Nothing
pJSNaN :: Parser JSVal
pJSNaN  = string "NaN" >> return JSNaN

-- | parse JS identifier for variable names.
-- it should start with letter or '$' or '_', and follow by zero or more
-- letters or digits or '$' or '_'
-- and it can't be any of the reserved words
--
-- Examples:
-- >>> parseMaybe pIdentifier "while"
-- Nothing
-- >>> parseMaybe pIdentifier "0abc"
-- Nothing
-- >>> parseMaybe pIdentifier "a123()"
-- Nothing
-- >>> parseMaybe pIdentifier "a123 "
-- Nothing
-- >>> parseMaybe pIdentifier "_xyz"
-- Just "_xyz"
-- >>> parseMaybe pIdentifier "a123"
-- Just "a123"
pIdentifier :: Parser String
pIdentifier  = do name <- identifierName
                  if name `elem` reservedWords
                    then failure [Unexpected name]
                    else return name
  where
    identifierName = do h <- identifierHead
                        t <- identifierTail
                        return (h : t)
    identifierHead = letterChar <|> oneOf "$_"
    identifierTail = many $ identifierHead <|> digitChar

-- | parse JS identifier for variable names.
-- then encapsulate it into a JSAtom
--
-- Examples:
-- >>> parseMaybe pJSAtom "while"
-- Nothing
-- >>> parseMaybe pJSAtom "0abc"
-- Nothing
-- >>> parseMaybe pJSAtom "_xyz"
-- Just _xyz
-- >>> parseMaybe pJSAtom "a123"
-- Just a123
pJSAtom :: Parser JSVal
pJSAtom  = JSAtom <$> try pIdentifier

-- | parse parameters in function declaration
--
-- Examples:
-- >>> parseMaybe pFunctionParams "( x , y ) "
-- Just ["x","y"]
-- >>> parseMaybe pFunctionParams "( )"
-- Just []
-- >>> parseMaybe pFunctionParams "(x,y)"
-- Just ["x","y"]
-- >>> parseMaybe pFunctionParams "(this)"
-- Nothing
-- >>> parseMaybe pFunctionPrincipalPart "( x , y ) { return x ; }"
-- Just (["x","y"],[return x])
pFunctionParams :: Parser [String]
pFunctionParams  = parentheses (sepBy (pIdentifier <* spaceConsumer) comma)
pFunctionBody :: Parser [JSStatement]
pFunctionBody  = braces (many pStatement)
pFunctionPrincipalPart :: Parser ([String], [JSStatement])
pFunctionPrincipalPart  = do params <- pFunctionParams
                             body   <- pFunctionBody
                             return (params, body)

-- | parser of js anonymous function
--
-- Examples:
-- >>> parseMaybe pJSFunction "function (x, y) { return x + y; }"
-- Just function(x,y){return (x+y);}
pJSFunction :: Parser JSVal
pJSFunction  =
  do begin          <- getPosition
     _              <- symbol "function"
     (params, body) <- pFunctionPrincipalPart
     end            <- getPosition
     return (JSFunction params body (convertPos begin) (convertPos end))

-- | parse a js value, it can be one of:
-- atom: an identifier for variable
-- boolean: true or false
-- null: representing null value
-- undefined: representing undefined value
-- int or float: both of them are js number
-- string: many chars surrounded by quotations
-- function: a js function object
--
-- Examples:
-- >>> parseMaybe pJSVal "true"
-- Just true
-- >>> parseMaybe pJSVal "truea"
-- Just truea
-- >>> parseMaybe pJSVal "null"
-- Just null
-- >>> parseMaybe pJSVal "undefined"
-- Just undefined
-- >>> parseMaybe pJSVal "1.3"
-- Just 1.3
-- >>> parseMaybe pJSVal "5"
-- Just 5
-- >>> parseMaybe pJSVal "'abc'"
-- Just abc
-- >>> parseMaybe pJSVal "x12"
-- Just x12
-- >>> parseMaybe pJSVal "function (x, y) {}"
-- Just function(x,y){}
pJSVal :: Parser JSVal
pJSVal  = try pJSAtom
      <|> try pJSBool
      <|> try pJSNaN
      <|> try pJSNull
      <|> try pJSUndefined
      <|> try pJSString
      <|> try pJSFloat
      <|> try pJSInt
      <|> pJSFunction

-- | skip all spaces and comments, including line comment started with // ,
-- and block comment surrounded by /* and */
--
-- Examples:
-- >>> parseMaybe spaceConsumer "  \n"
-- Just ()
-- >>> parseMaybe spaceConsumer " // abc\n  \n/*xyz*/  "
-- Just ()
-- >>> parseMaybe spaceConsumer "a  "
-- Nothing
-- >>> parseMaybe spaceConsumer " /**/ a"
-- Nothing
spaceConsumer :: Parser ()
spaceConsumer  = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

-- | space consumer for between function that skip many space chars
--
-- Examples:
-- >>> parseMaybe (symbol "#") " #"
-- Nothing
-- >>> parseMaybe (symbol "#") "#"
-- Just "#"
-- >>> parseMaybe (symbol "#") "# "
-- Just "#"
symbol :: String -> Parser String
symbol  = L.symbol spaceConsumer

-- | convert a expression to surrounded by parentheses
--
-- Examples:
-- >>> parseTest (parentheses pJSVal) "(abc)"
-- abc
-- >>> parseMaybe (parentheses pJSVal) "( abc) "
-- Just abc
-- >>> parseMaybe (parentheses pJSVal) "(abc )"
-- Nothing
parentheses :: Parser a -> Parser a
parentheses  = between (symbol "(") (symbol ")")
-- | convert a expression to surrounded by braces
braces      :: Parser a -> Parser a
braces       = between (symbol "{") (symbol "}")
-- | convert a expression to surrounded by brackets
brackets    :: Parser a -> Parser a
brackets     = between (symbol "[") (symbol "]")
-- | parse a semicolon symbol
--
-- Examples:
-- >>> parseMaybe semicolon ";  "
-- Just ";"
-- >>> parseMaybe semicolon " ;"
-- Nothing
semicolon   :: Parser String
semicolon    = symbol ";"
-- | parse a comma symbol
comma       :: Parser String
comma        = symbol ","
-- | parse a colon symbol
colon       :: Parser String
colon        = symbol ":"
-- | parse a dot symbol
dot         :: Parser String
dot          = symbol "."

-- | parse a semicolon symbol or nothing
--
-- Examples:
-- >>> parseMaybe maybeSemicolon ";  "
-- Just (Just ";")
-- >>> parseMaybe maybeSemicolon ""
-- Just Nothing
-- >>> parseMaybe maybeSemicolon " ;"
-- Nothing
maybeSemicolon :: Parser (Maybe String)
maybeSemicolon  = (do s <- semicolon
                      return (Just s)) <|> return Nothing

-- | parse a js expression
--
-- Examples:
-- >>> parseMaybe pExpression "x "
-- Just x
-- >>> parseMaybe pExpression "( x ) "
-- Just x
-- >>> parseMaybe pExpression "f (x) "
-- Just f(x)
-- >>> parseMaybe pExpression "f(x)(y, z)"
-- Just f(x)(y,z)
-- >>> parseMaybe pExpression "f(x)['abc']"
-- Just f(x)[abc]
-- >>> parseMaybe pExpression "x = f(a)"
-- Just x = f(a)
-- >>> parseMaybe pExpression "x = f(a) + 3 "
-- Just x = (f(a)+3)
-- >>> parseMaybe pExpression "x + 5 + f(3) "
-- Just ((x+5)+f(3))
-- >>> parseMaybe pExpression "[ 1 , 2, 3 ] [ 2 ] "
-- Just [1,2,3][2]
pExpression :: Parser JSExpression
pExpression  = try pAssignmentExpression
           <|> pOperateExpression

-- | row value expression
--
-- Examples:
-- >>> parseMaybe pRowValExpression "x "
-- Just x
-- >>> parseMaybe pRowValExpression "'abc' "
-- Just abc
pRowValExpression :: Parser JSExpression
pRowValExpression = (JSRowVal <$> pJSVal) <* spaceConsumer

-- | parse a expression that can occur on the left side of a assignment
--
-- Examples:
-- >>> parseMaybe pLeftSideExpression "x "
-- Just x
-- >>> parseMaybe pLeftSideExpression "( x ) "
-- Just x
-- >>> parseMaybe pLeftSideExpression "f (x) "
-- Just f(x)
-- >>> parseMaybe pLeftSideExpression "f(x)(y, z)"
-- Just f(x)(y,z)
-- >>> parseMaybe pLeftSideExpression "f(x)['abc']"
-- Just f(x)[abc]
pLeftSideExpression :: Parser JSExpression
pLeftSideExpression  = do expr <-  try (parentheses pLeftSideExpression)
                               <|> pRowValExpression
                          furtherExpression expr

-- | terms for expression with operator
--
-- Examples:
-- >>> parseMaybe pOperateTerm "x "
-- Just x
-- >>> parseMaybe pOperateTerm "( x ) "
-- Just x
-- >>> parseMaybe pOperateTerm "f (x) "
-- Just f(x)
-- >>> parseMaybe pOperateTerm "f(x)(y, z)"
-- Just f(x)(y,z)
-- >>> parseMaybe pOperateTerm "f(x)['abc']"
-- Just f(x)[abc]
-- >>> parseMaybe pOperateTerm "[ 1 , 2 , 3 ]"
-- Just [1,2,3]
pOperateTerm :: Parser JSExpression
pOperateTerm  = do expr <-  try (parentheses pExpression)
                        <|> try pObjectLiterialExpression
                        <|> try pArrayLiterialExpression
                        <|> pRowValExpression
                   furtherExpression expr

-- | look ahead to find if there is further expression can be parsed,
-- if there is anything can be parsed, then it will parse the further
-- expression, such as function call, field get(including dot or brackets),
-- then it will build a new expression with the input expression and the
-- parsing result, after that, it will look ahead by calling itself.
-- otherwise, it will just return the input expression
--
-- Examples:
-- >>> let parser = furtherExpression (JSRowVal (JSAtom "f"))
-- >>> parseMaybe parser "(a, 1, 2 + 3)"
-- Just f(a,1,(2+3))
-- >>> let parser = furtherExpression (JSRowVal (JSAtom "f"))
-- >>> parseMaybe parser "(a)(2)['xyz']"
-- Just f(a)(2)[xyz]
-- >>> let parser = furtherExpression (JSRowVal (JSAtom "f"))
-- >>> parseMaybe parser "a)"
-- Nothing
furtherExpression     :: JSExpression -> Parser JSExpression
furtherExpression expr = lookAheadExpression <|> return expr
  where
    lookAheadExpression :: Parser JSExpression
    lookAheadExpression  =
      do c <- lookAhead (oneOf "[(.")
         case c of
           '[' -> brcketField
           '(' -> funcCall
           '.' -> dotField
        where
          brcketField = do inside <- brackets pExpression
                           furtherExpression (JSBracketFieldGet expr inside)
          funcCall = do inside <- parentheses (sepBy pExpression comma)
                        furtherExpression (JSFunctionCall expr inside)
          dotField = do after <- pRowValExpression
                        furtherExpression (JSDotFieldGet expr after)

-- | operators allowed in operator expression
operators :: [[E.Operator Parser JSExpression]]
operators =
  [ [ E.Postfix (symbol "++" *> pure (JSSuffixUnaryOperate JSIncrement))
    , E.Postfix (symbol "--" *> pure (JSSuffixUnaryOperate JSDecrement))
    ]
  , [ E.Prefix  (symbol "++"  *> pure (JSPrefixUnaryOperate JSIncrement))
    , E.Prefix  (symbol "--"  *> pure (JSPrefixUnaryOperate JSDecrement))
    ]
  , [ E.InfixL  (symbol "*"   *> pure (JSInfixOperate JSMultiplication))
    , E.InfixL  (symbol "/"   *> pure (JSInfixOperate JSDivision))
    , E.InfixL  (symbol "%"   *> pure (JSInfixOperate JSModulus))
    ]
  , [ E.InfixL  (symbol "+"   *> pure (JSInfixOperate JSAddition))
    , E.InfixL  (symbol "-"   *> pure (JSInfixOperate JSSubtraction))
    ]
  , [ E.InfixL  (symbol "<="  *> pure (JSInfixOperate JSLessOrEqual))
    , E.InfixL  (symbol "<"   *> pure (JSInfixOperate JSLessThan))
    , E.InfixL  (symbol ">="  *> pure (JSInfixOperate JSGreaterOrEqual))
    , E.InfixL  (symbol ">"   *> pure (JSInfixOperate JSGreaterThan))
    ]
  , [ E.InfixL  (symbol "=="  *> pure (JSInfixOperate JSEqualTo))
    , E.InfixL  (symbol "!="  *> pure (JSInfixOperate JSNotEqual))
    , E.InfixL  (symbol "===" *> pure (JSInfixOperate JSEqualValAndType))
    , E.InfixL  (symbol "!==" *> pure (JSInfixOperate JSNotEqualValOrType))
    ]
  , [ E.Prefix  (symbol "!"   *> pure (JSPrefixUnaryOperate JSLogicNot))]
  , [ E.InfixL  (symbol "&&"  *> pure (JSInfixOperate JSLogicAnd))
    , E.InfixL  (symbol "||"  *> pure (JSInfixOperate JSLogicOr))
    ]
  ]

-- | parse js expression with operators
--
-- Examples:
-- >>> parseTest pOperateExpression "3 + 5"
-- (3+5)
-- >>> parseTest pOperateExpression "1 + 3 * 5"
-- (1+(3*5))
-- >>> parseTest pOperateExpression "true && 3"
-- (true&&3)
-- >>> parseMaybe pOperateExpression "5 == false "
-- Just (5==false)
-- >>> parseMaybe pOperateExpression "5 ++"
-- Just 5++
-- >>> parseMaybe pOperateExpression "++x + 3"
-- Just (++x+3)
pOperateExpression :: Parser JSExpression
pOperateExpression  = E.makeExprParser pOperateTerm operators

-- | parse a assignment expression
--
-- Examples:
-- >>> parseMaybe pAssignmentExpression "x = 5"
-- Just x = 5
-- >>> parseMaybe pAssignmentExpression "x = f(a)"
-- Just x = f(a)
pAssignmentExpression :: Parser JSExpression
pAssignmentExpression  = do name  <- pLeftSideExpression
                            _     <- spaceConsumer >> symbol "="
                            value <- pExpression
                            return (JSAssignment name value)

-- | parse a js object written in json literial
--
-- Examples:
-- >>> parseMaybe pObjectLiterialExpression "{ x : 5, y: 'abc' } "
-- Just {x,y}
-- >>> parseMaybe pObjectLiterialExpression "{ } "
-- Just {}
-- >>> parseMaybe pObjectLiterialExpression "{ x } "
-- Nothing
pObjectLiterialExpression :: Parser JSExpression
pObjectLiterialExpression  =
  JSObjectLiteral <$> (braces (sepBy pEntry comma) <* spaceConsumer)
  where
    pEntry :: Parser (String, JSExpression)
    pEntry = do name    <- pIdentifier
                _       <- spaceConsumer >> colon
                content <- pExpression
                return (name, content)

-- | parse a js array written in array literial
--
-- Examples:
-- >>> parseMaybe pArrayLiterialExpression "[ x , 10 ] "
-- Just [x,10]
-- >>> parseMaybe pArrayLiterialExpression "[ ] "
-- Just []
pArrayLiterialExpression :: Parser JSExpression
pArrayLiterialExpression  =
  JSArrayLiteral <$> (brackets (sepBy pExpression comma) <* spaceConsumer)

-- | convert a pos to a (Int, Int) representing (line, column)
convertPos  :: SourcePos -> (Int, Int)
convertPos p = (sourceLine p, sourceColumn p)

-- | add position information to js statement content parser
-- convert the parser to a js statement parser
--
-- Examples:
-- >>> let s = "if ( x < 5 ) {  x ; } else { y ; } "
-- >>> parseMaybe pIfStatement s
-- Just if((x<5)){x;}else{y;}
-- >>> let s = "for(var x = 3; x < 5; x = x+1) { f(x); }"
-- >>> parseMaybe pForStatement s
-- Just for(var x=3;(x<5);x = (x+1)){f(x);}
positionStatement   :: Parser JSStatementContent -> Parser JSStatement
positionStatement pc = do begin   <- getPosition
                          content <- pc
                          end     <- getPosition
                          return JSStatement
                                 { statementContnet  = content
                                 , statementPosBegin = convertPos begin
                                 , statementPosEnd   = convertPos end
                                 }

-- | parse a statement
--
-- Examples:
-- >>> let parse = parseString pStatement
-- >>> parse "for (var x = 5; x <= 3; x ++) { var y = 5; }"
-- Right for(var x=5;(x<=3);x++){var y=5;}
pStatement :: Parser JSStatement
pStatement  = try pEmptyStatement
          <|> try pDeclarationStatement
          <|> try pIfStatement
          <|> try pForStatement
          <|> try pBlockStatement
          <|> try pBreakStatement
          <|> try pContinueStatement
          <|> try pReturnStatement
          <|> pRowExprStatement

-- | parse a statement which is a row expression
--
-- Examples:
-- >>> parseMaybe pRowExprStatementContent "x "
-- Just x
-- >>> parseMaybe pRowExprStatementContent "x ; "
-- Just x
pRowExprStatementContent :: Parser JSStatementContent
pRowExprStatementContent  = JSRowExpression <$> (pExpression <* maybeSemicolon)
pRowExprStatement :: Parser JSStatement
pRowExprStatement = positionStatement pRowExprStatementContent

-- | parse js if-else statement content
--
-- Examples:
-- >>> parseMaybe pIfStatementContent "if ( x < 5 ) {  x ; } else { y ; } "
-- Just if((x<5)){x;}else{y;}
-- >>> parseMaybe pIfStatementContent "if ( x < 5 ) {  x ; } "
-- Just if((x<5)){x;}
-- >>> parseMaybe pIfStatementContent "if ( x < 5 ) {  } "
-- Just if((x<5)){}
pIfStatementContent :: Parser JSStatementContent
pIfStatementContent = do _         <- symbol "if"
                         expr      <- parentheses pExpression
                         ifStmts   <- pStatement
                         maybeElse <- pElse
                         return (JSIf expr ifStmts maybeElse)
  where
    pNoElse :: Parser (Maybe JSStatement)
    pNoElse  = return Nothing
    pHasElse :: Parser (Maybe JSStatement)
    pHasElse  = Just <$> (symbol "else" >> pStatement)
    pElse = try pHasElse <|> pNoElse
pIfStatement :: Parser JSStatement
pIfStatement  = positionStatement pIfStatementContent

-- | parse a js for loop statement content
--
-- Examples:
-- >>> let s = "for(var x = 3; x < 5; x = x+1) { f(x); }"
-- >>> parseMaybe pForStatementContent s
-- Just for(var x=3;(x<5);x = (x+1)){f(x);}
-- >>> let s = "for ( var x=3; x < 5; x=x+1 ) { f(x); } "
-- >>> parseMaybe pForStatementContent s
-- Just for(var x=3;(x<5);x = (x+1)){f(x);}
-- >>> let s = "for ( ; ; ) { f(x); } "
-- >>> parseMaybe pForStatementContent s
-- Just for(;;){f(x);}
pForStatementContent :: Parser JSStatementContent
pForStatementContent  =
  do _     <- symbol "for" >> symbol "("
     begin <- sepBy pStatement comma
     cond  <- sepBy pStatement comma
     after <- sepBy pStatement comma <* symbol ")"
     body  <- pStatement
     return  JSForLoop { beginLoop     = begin
                       , condition     = cond
                       , afterEachTime = after
                       , forLoopBody   = body
                       }
pForStatement :: Parser JSStatement
pForStatement  = positionStatement pForStatementContent

-- | parse a js block, it is many statements surrounded by braces
--
-- Examples:
-- >>> parseMaybe pBlockStatementContent "{ y = 5\nx + 3; return 5; } "
-- Just {y = 5;(x+3);return 5;}
-- >>> parseMaybe pBlockStatementContent "{}"
-- Just {}
-- >>> parseMaybe pBlockStatementContent "{ {} x-5; }"
-- Just {{};(x-5);}
pBlockStatementContent :: Parser JSStatementContent
pBlockStatementContent  = JSBlock <$> braces (many pStatement)
pBlockStatement :: Parser JSStatement
pBlockStatement  = positionStatement pBlockStatementContent

-- | parse a js empty statement which is only a semicolon
pEmptyStatementContent :: Parser JSStatementContent
pEmptyStatementContent  = const JSEmptyStatement <$> semicolon
pEmptyStatement :: Parser JSStatement
pEmptyStatement  = positionStatement pEmptyStatementContent
-- | parse a js break statement which is represented by "break"
pBreakStatementContent :: Parser JSStatementContent
pBreakStatementContent  =
  const JSBreak <$> (symbol "break" <* maybeSemicolon)
pBreakStatement :: Parser JSStatement
pBreakStatement  = positionStatement pBreakStatementContent
-- | parse a js continue statement which is represented by "continue"
pContinueStatementContent :: Parser JSStatementContent
pContinueStatementContent  =
  const JSContinue <$> (symbol "continue" <* maybeSemicolon)
pContinueStatement :: Parser JSStatement
pContinueStatement  = positionStatement pContinueStatementContent

-- | parse js return statement, it may return nothing or just a expression
--
-- Examples:
-- >>> parseMaybe pReturnStatementContent "return "
-- Just return
-- >>> parseMaybe pReturnStatementContent "return;"
-- Just return
-- >>> parseMaybe pReturnStatementContent "return x+3 "
-- Just return (x+3)
-- >>> parseMaybe pReturnStatementContent "return x+3 ;"
-- Just return (x+3)
pReturnStatementContent :: Parser JSStatementContent
pReturnStatementContent  =
  JSReturn <$> ((symbol "return" >> returnContent) <* maybeSemicolon)
  where
    noContent :: Parser (Maybe JSExpression)
    noContent  = return Nothing
    hasContent :: Parser (Maybe JSExpression)
    hasContent  = Just <$> pExpression
    returnContent :: Parser (Maybe JSExpression)
    returnContent  = try hasContent <|> noContent
pReturnStatement :: Parser JSStatement
pReturnStatement  = positionStatement pReturnStatementContent

-- | parse a named function declaration
--
-- Examples:
-- >>> let s = "function f ( x , y ) { x = 3 ; return x + y ; }"
-- >>> parseMaybe pFuncDeclStatementContent s
-- Just function f(x,y){x = 3;return (x+y);}
pFuncDeclStatementContent :: Parser JSStatementContent
pFuncDeclStatementContent  =
  do begin          <- getPosition
     _              <- symbol "function"
     name           <- pIdentifier
     _              <- spaceConsumer
     (params, body) <- pFunctionPrincipalPart
     end            <- getPosition
     return $ JSFuncDeclaration name
              (JSFunction params body (convertPos begin) (convertPos end))

-- | parse a variables declaration, it may has a initial value
--
-- Examples:
-- >>> let s = "var x = 5 + f (3) ; "
-- >>> parseMaybe pVarDeclStatementContent s
-- Just var x=(5+f(3))
-- >>> let s = "var x = 5 + f (3) "
-- >>> parseMaybe pVarDeclStatementContent s
-- Just var x=(5+f(3))
-- >>> let s = "var x"
-- >>> parseMaybe pVarDeclStatementContent s
-- Just var x
-- >>> let s = "var x ; "
-- >>> parseMaybe pVarDeclStatementContent s
-- Just var x
pVarDeclStatementContent :: Parser JSStatementContent
pVarDeclStatementContent  = try hasInit <|> noInit
  where
    hasInit = do _    <- symbol "var"
                 name <- pIdentifier
                 _    <- spaceConsumer >> symbol "="
                 expr <- pExpression <* maybeSemicolon
                 return (JSDeclaration name (Just expr))
    noInit = do _    <- symbol "var"
                name <- pIdentifier
                _    <- spaceConsumer >> maybeSemicolon
                return (JSDeclaration name Nothing)

-- | parse a declaration statement, it can be either a variable declaration
-- or a function declaration
--
-- Examples:
-- >>> let s = "function f ( x , y ) { x = 3 ; return x + y ; }"
-- >>> parseMaybe pDeclarationStatementContent s
-- Just function f(x,y){x = 3;return (x+y);}
-- >>> let s = "var x = 5 + f (3) "
-- >>> parseMaybe pDeclarationStatementContent s
-- Just var x=(5+f(3))
-- >>> let s = "var x"
-- >>> parseMaybe pDeclarationStatementContent s
-- Just var x
pDeclarationStatementContent :: Parser JSStatementContent
pDeclarationStatementContent  = try pFuncDeclStatementContent
                            <|> pVarDeclStatementContent
pDeclarationStatement :: Parser JSStatement
pDeclarationStatement  = positionStatement pDeclarationStatementContent

-- | parse a js program source code
--
-- Examples:
-- >>> parseMaybe jsProgramParser "var x = 3\nfunction x(y, z) { return y+z; } "
-- Just [var x=3,function x(y,z){return (y+z);}]
jsProgramParser :: Parser [JSStatement]
jsProgramParser  = spaceConsumer >> many pStatement

-- | parse a js expression
jsExpressionParser :: Parser JSExpression
jsExpressionParser  = pExpression

-- | parse a js value
jsValueParser :: Parser JSVal
jsValueParser  = pJSVal

-- | parse a js statement
jsStatementParser :: Parser JSStatement
jsStatementParser = pStatement

-- | parse a string
--
-- Examples:
-- >>> parseString jsProgramParser "var x = 3\nfunction x(y, z) { return y+z; } "
-- Right [var x=3,function x(y,z){return (y+z);}]
-- >>> parseString jsProgramParser "var x = 3\nfunction x(y, ) { return y+z; } "
-- Left Parse error at JavaScript:2:15:
-- unexpected ')'
-- expecting letter
-- >>> parseString jsStatementParser "[x, 3, function(){}]"
-- Right [x,3,function(){}]
parseString :: Parser a -> String -> ThrowsError a
parseString parser input = case parse parser "JavaScript" input of
  Left err  -> throwError $ ParserError err
  Right val -> return val
