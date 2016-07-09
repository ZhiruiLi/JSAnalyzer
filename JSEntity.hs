module JSEntity
( JSEnv
, JSScope
, JSProgram
, JSStatement (..)
, JSStatementContent (..)
, JSExpression (..)
, JSOperator (..)
, JSVal (..)
) where

import           Data.IORef
import           Data.List  (intercalate, sortOn)

-- | the JS runtime environment
-- when the code run into a code block,
-- a new scope will be generate
type JSEnv = [JSScope]

-- | the JS runtime scope
-- storing key-value pairs for variables in the scope
type JSScope = IORef [(String, IORef JSVal)]

-- | the whole JS program
type JSProgram = [JSStatement]

-- | transform a list of values to String, concat them with String i
-- intercalate, then surround it with String h and String t
--
-- Examples:
-- >>> surroundCalate show ", " "{" "}" []
-- "{}"
-- >>> surroundCalate show ", " "{" "}" ["a","b"]
-- "{\"a\", \"b\"}"
-- >>> surroundCalate id ", " "{" "}" ["a","b"]
-- "{a, b}"
surroundCalate :: (a -> String)
               -> String
               -> String
               -> String
               -> [a]
               -> String
surroundCalate toS i h t as =
  h `mappend` intercalate i (fmap toS as) `mappend` t

-- | behaves just like surroundCalate, but the last item will also follow
-- by the String i
--
-- Examples:
-- >>> surroundCalateEnd show ", " "{" "}" []
-- "{}"
-- >>> surroundCalateEnd show ", " "{" "}" ["a","b"]
-- "{\"a\", \"b\", }"
-- >>> surroundCalateEnd id ", " "{" "}" ["a","b"]
-- "{a, b, }"
surroundCalateEnd :: (a -> String)
                  -> String
                  -> String
                  -> String
                  -> [a]
                  -> String
surroundCalateEnd _   _ h t [] = h `mappend` t
surroundCalateEnd toS i h t as =
  h `mappend` intercalate i (fmap toS as) `mappend` i `mappend` t

data JSStatement =
  JSStatement { statementContnet  :: JSStatementContent
              , statementPosBegin :: (Int, Int)
              , statementPosEnd   :: (Int, Int)
              }

instance Eq JSStatement where
  JSStatement {statementPosBegin = b1, statementPosEnd = e1} ==
    JSStatement {statementPosBegin = b2, statementPosEnd = e2} =
      (b1 == b2) && (e1 == e2)

instance Show JSStatement where
  show JSStatement { statementContnet = c } = show c
    -- show b `mappend` "-" `mappend` show e `mappend` ": " `mappend` show c

data JSStatementContent =
                   -- x
                   JSRowExpression JSExpression
                   -- var x = 3
                   -- var f = function(x) { JSStatements }
                   -- var y
                 | JSDeclaration String (Maybe JSExpression)
                   -- function f(x) { JSStatements }
                 | JSFuncDeclaration String JSVal
                   -- if (JSExpression) { JSStatements } else { JSStatements }
                   -- if (JSExpression) { JSStatements }
                 | JSIf JSExpression JSStatement (Maybe JSStatement)
                   -- for (beginLoop; condition; afterEachTime) JSCodeBlock
                 | JSForLoop { beginLoop     :: [JSStatement]
                             , condition     :: [JSStatement]
                             , afterEachTime :: [JSStatement]
                             , forLoopBody   :: JSStatement
                             }
                   -- break
                 | JSBreak
                   -- continue
                 | JSContinue
                   -- return
                   -- return 3
                 | JSReturn (Maybe JSExpression)
                   -- { JSStatements }
                 | JSBlock [JSStatement]
                 | JSEmptyStatement

instance Show JSStatementContent where
  show (JSRowExpression expr) = show expr
  show (JSDeclaration s maybeExpr) = "var " `mappend` s `mappend` t
    where t = case maybeExpr of
                Nothing   -> ""
                Just expr -> "=" `mappend` show expr
  show (JSFuncDeclaration name
        JSFunction { functionParams = p, functionBody = b }) =
    "function " `mappend` name `mappend`
    showParamters p `mappend` showFunctionBody b
  show (JSFuncDeclaration name val) =
    "invalid <function> " `mappend` name `mappend` " = " `mappend` show val
  show (JSIf expr trueBranch maybeFalseBranch) =
    "if(" `mappend` show expr  `mappend` ")" `mappend`
    show trueBranch `mappend`
    case maybeFalseBranch of
      Nothing -> ""
      Just fb -> "else" `mappend` show fb
  show JSForLoop { beginLoop = b
                 , condition = c
                 , afterEachTime = a
                 , forLoopBody = f } =
    "for" `mappend`
    surroundCalate id ";" "(" ")" inParans `mappend`
    show f
    where
      notEmpty JSStatement {statementContnet = JSEmptyStatement} = False
      notEmpty _ = True
      inParans = concatWithColon <$> (filter notEmpty <$> [b, c, a])
      concatWithColon x = intercalate "," (show <$> x)
  show JSBreak = "break"
  show JSContinue = "continue"
  show (JSReturn maybeExpr) = "return" `mappend` returnTail
    where
      returnTail = case maybeExpr of
                     Nothing -> ""
                     Just x  -> " " `mappend` show x
  show (JSBlock stmts) =
    surroundCalateEnd show ";" "{" "}" stmts
  show JSEmptyStatement = ";"

data JSExpression =
                    -- x
                    -- 3
                    -- "str"
                    -- function (x) { return x + 1 }
                    JSRowVal JSVal
                    -- x = 3
                  | JSAssignment JSExpression JSExpression
                    -- x + 3
                  | JSInfixOperate JSOperator JSExpression JSExpression
                    -- x++
                  | JSSuffixUnaryOperate JSOperator JSExpression
                    -- ++x
                  | JSPrefixUnaryOperate JSOperator JSExpression
                    -- f(x, 3)
                  | JSFunctionCall JSExpression [JSExpression]
                    -- a.field
                  | JSDotFieldGet JSExpression JSExpression
                    -- a["field"]
                  | JSBracketFieldGet JSExpression JSExpression
                    -- { a: 1 + 3 }
                  | JSObjectLiteral [(String, JSExpression)]
                    -- [1, 2, 3]
                  | JSArrayLiteral [JSExpression]

instance Show JSExpression where
  show (JSRowVal val)                 = show val
  show (JSAssignment left right)      =
    show left `mappend` " = " `mappend` show right
  show (JSInfixOperate o expr1 expr2) =
    "(" `mappend` show expr1 `mappend` show o `mappend` show expr2 `mappend` ")"
  show (JSSuffixUnaryOperate o expr)  = show expr `mappend` show o
  show (JSPrefixUnaryOperate o expr)  = show o `mappend` show expr
  show (JSFunctionCall name params)   =
    show name `mappend` surroundCalate show "," "(" ")" params
  show (JSDotFieldGet obj field)      =
    show obj `mappend` "." `mappend` show field
  show (JSBracketFieldGet obj field)  =
    show obj `mappend` "[" `mappend` show field `mappend` "]"
  show (JSObjectLiteral kvs)          =
    surroundCalate id "," "{" "}" (fst <$> kvs)
  show (JSArrayLiteral arr)           =
    surroundCalate id "," "[" "]" (show <$> arr)

data JSOperator = JSAddition          -- +
                | JSSubtraction       -- -
                | JSMultiplication    -- \*
                | JSDivision          -- /
                | JSModulus           -- %
                | JSEqualTo           -- ==
                | JSNotEqual          -- !=
                | JSEqualValAndType   -- ===
                | JSNotEqualValOrType -- !==
                | JSGreaterThan       -- >
                | JSLessThan          -- <
                | JSGreaterOrEqual    -- >=
                | JSLessOrEqual       -- <=
                | JSLogicAnd          -- &&
                | JSLogicOr           -- ||
                | JSLogicNot          -- !
                | JSIncrement         -- ++
                | JSDecrement         -- --
  deriving (Eq)

instance Show JSOperator where
  show JSAddition          = "+"
  show JSSubtraction       = "-"
  show JSMultiplication    = "*"
  show JSDivision          = "/"
  show JSModulus           = "%"
  show JSEqualTo           = "=="
  show JSNotEqual          = "!="
  show JSEqualValAndType   = "==="
  show JSNotEqualValOrType = "!=="
  show JSGreaterThan       = ">"
  show JSLessThan          = "<"
  show JSGreaterOrEqual    = ">="
  show JSLessOrEqual       = "<="
  show JSLogicAnd          = "&&"
  show JSLogicOr           = "||"
  show JSLogicNot          = "!"
  show JSIncrement         = "++"
  show JSDecrement         = "--"

data JSVal =
             -- true
             JSBool Bool
             -- 1
           | JSInt Integer
             -- 2.1
           | JSFloat Double
             -- "abc"
             -- 'str'
           | JSString String
             -- function (x) { JSStatements }
           | JSFunction { functionParams   :: [String]
                        , functionBody     :: [JSStatement]
                        , functionBeginPos :: (Int, Int)
                        , functionEndPos   :: (Int, Int)
                        }
             -- function with environment it defined
           | JSClosure JSVal JSEnv
             -- [x, y, 1]
           | JSArray [JSVal]
             -- document
           | JSObject [(String, JSVal)]
             -- x
           | JSAtom String
             -- null
           | JSNull
             -- undefined
           | JSUndefined
             -- this
           | JSThis
             -- not a number
           | JSNaN

instance Show JSVal where
  show (JSBool True)  = "true"
  show (JSBool False) = "false"
  show (JSInt n)      = show n
  show (JSFloat f)    = show f
  show (JSString s)   = s
  show (JSAtom s)     = s
  show (JSArray arr)  = surroundCalate show "," "" "" arr
  show (JSObject _)   = "[object Object]"
  show JSNull         = "null"
  show JSUndefined    = "undefined"
  show JSThis         = "this"
  show JSNaN          = "NaN"
  show JSFunction { functionParams = p, functionBody = b } =
    "function" `mappend` showParamters p `mappend` showFunctionBody b
  show (JSClosure func _) = show func

showParamters :: [String] -> String
showParamters = surroundCalate id "," "(" ")"

showFunctionBody :: [JSStatement] -> String
showFunctionBody = surroundCalateEnd show ";" "{" "}"

instance Eq JSVal where
  (JSBool b1)          == (JSBool b2)           = b1 == b2
  (JSInt i1)           == (JSInt i2)            = i1 == i2
  (JSFloat f1)         == (JSFloat f2)          = f1 == f2
  (JSInt i1)           == (JSFloat f2)          = fromInteger i1 == f2
  (JSFloat f1)         == (JSInt i2)            = f1 == fromInteger i2
  (JSString s1)        == (JSString s2)         = s1 == s2
  (JSArray a1)         == (JSArray a2)          = a1 == a2
  (JSAtom a1)          == (JSAtom a2)           = a1 == a2
  JSNull               == JSNull                = True
  JSUndefined          == JSUndefined           = True
  JSThis               == JSThis                = True
  JSNaN                == JSNaN                 = False
  JSFunction _ _ b1 e1 == JSFunction _ _ b2 e2  = b1 == b2 && e1 == e2
  (JSClosure f1 _)     == (JSClosure f2 _)      = f1 == f2
  (JSObject o1)        == (JSObject o2)         = sortOn fst o1 == sortOn fst o2
  _                    == _                     = False
