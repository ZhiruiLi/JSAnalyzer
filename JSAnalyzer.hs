module JSAnalyzer
( analyzeProgram
, WarningItem
, AnalyzeResult
) where

import           Control.Monad (void)
import           Data.IORef
import           JSEntity
import           JSError
import           JSEvaluator
import           JSParser

data WarningItem = UncalledFunction JSVal
                 | UnreachStatement JSStatement

instance Show WarningItem where
  show (UncalledFunction func@JSFunction { functionBeginPos = (line, _) }) =
    "function never been called at line: " `mappend`
    show line `mappend` " -- " `mappend` show func
  show (UnreachStatement stmt@JSStatement { statementPosBegin = (line, _) }) =
    "can't reach statement at line: " `mappend`
    show line `mappend` " -- " `mappend` show stmt
  show _ = "Invalid Warrning"

type AnalyzeResult = [WarningItem]

-- | analyze the whole program
-- the result will be the list of warnings
--
-- Examples:
-- >>> let parse = parseString jsProgramParser
-- >>> let s = "function(x) { return x; } var x = 3; if (x < 2) { x = 5; } x = 3;"
-- >>> let (Right prog) = parse s
-- >>> :{
--  do env <- initEnv []
--     res <- runExceptT $ analyzeProgram env prog
--     print res
-- :}
-- Right [function never been called at line: 1 -- function(x){return x;},can't reach statement at line: 1 -- return x,can't reach statement at line: 1 -- {x = 5;},can't reach statement at line: 1 -- x = 5]
analyzeProgram :: JSEnv -> JSProgram -> IOThrowsError AnalyzeResult
analyzeProgram initEnv program =
  do record <- liftIO $ initProgramFlowRecord program
     sType  <- analyzeStatements record initEnv program
     case sType of
       NormalStatement _ -> let (fCountRef, sCountRef) = record in
         do fCount <- liftIO $ readCounts fCountRef
            sCount <- liftIO $ readCounts sCountRef
            let uncallFuncWarns  = (UncalledFunction . fst) <$>
                                   filter ((== 0) . snd) fCount
            let unreachStmtWarns = (UnreachStatement . fst) <$>
                                   filter ((== 0) . snd) sCount
            return $ uncallFuncWarns `mappend` unreachStmtWarns
       _ -> throwError $ invalidStatementType sType

type FunctionCallCount = (JSVal, IORef Int)
type StatementRunCount = (JSStatement, IORef Int)

type ProgramFlowRecord = ([FunctionCallCount], [StatementRunCount])

type TraverseTempResult = ([JSVal], [JSStatement])

readCounts :: [(a, IORef b)] -> IO [(a, b)]
readCounts counts = do lst <- sequence [readIORef ref | (_, ref) <- counts]
                       return $ zip (fst <$> counts) lst

-- | helper function to print the counts
printCountNums :: [(a, IORef Int)] -> IO ()
printCountNums counts =
  do lst <- sequence [readIORef ref | (_, ref) <- counts]
     print lst

-- | helper function to print the detail of counts
printCountDetails :: Show a => [(a, IORef Int)] -> IO ()
printCountDetails counts =
  do lst <- sequence [readIORef ref | (_, ref) <- counts]
     print $ zip (fst <$> counts) lst

-- | traverse the program, find all statements and function definitions,
-- record them in a pair
--
-- Examples:
-- >>> let parse = parseString jsProgramParser
-- >>> let s = "function f1 (x) { function f2() { return x} var y = 3; return y; } var z = f1; z(); z = 5; z ++;"
-- >>> let (Right program) = parse s
-- >>> :{
--  do (fcounts, scounts) <- initProgramFlowRecord program
--     let fs = map fst fcounts
--     let ss = map fst scounts
--     print (length fs, length ss)
-- :}
-- (2,9)
initProgramFlowRecord :: JSProgram -> IO ProgramFlowRecord
initProgramFlowRecord  = traverseProgram ([], [])
  where
    count :: a -> IO (a, IORef Int)
    count x = do i <- newIORef 0
                 return (x, i)
    counts       :: [a] -> IO [(a, IORef Int)]
    counts []     = return []
    counts (x:xs) = do xc  <- count x
                       xcs <- counts xs
                       return (xc:xcs)
    traverseProgram :: ProgramFlowRecord -> [JSStatement] -> IO ProgramFlowRecord
    traverseProgram result           []           = return result
    traverseProgram (fCount, sCount) (stmt:stmts) =
      do let (fs, ss) = traverseStatement stmt
         fCount'  <- counts fs
         sCount'  <- counts ss
         traverseProgram (fCount `mappend` fCount',
                          sCount `mappend` sCount') stmts

valToStmtContent :: JSVal -> JSStatementContent
valToStmtContent = JSRowExpression . JSRowVal

-- | concat corresponding list in two pairs of lists
--
-- Examples:
-- >>> concatPair ([], []) ([], [])
-- ([],[])
-- >>> concatPair ([1], [2]) ([3], [4])
-- ([1,3],[2,4])
concatPair :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatPair (as, bs) (as', bs') = (as `mappend` as', bs `mappend` bs')

-- | fold the list of pairs of list using concatPair
--
-- Examples:
-- >>> concatPairs []
-- ([],[])
-- >>> concatPairs [([1],[2])]
-- ([1],[2])
-- >>> concatPairs [([1], [2]), ([3], [4]), ([5], [6])]
-- ([1,3,5],[2,4,6])
concatPairs :: [([a], [b])] -> ([a], [b])
concatPairs = foldl concatPair ([], [])

-- | traverse an expression to find all function declaration and statements
--
-- Examples:
-- >>> let parse = parseString jsExpressionParser
-- >>> let (Right e1) = parse "3"
-- >>> traverseExpression e1
-- ([],[])
-- >>> let (Right e2) = parse "function (x, y) { var z = 3; return (x + y); }"
-- >>> traverseExpression e2
-- ([function(x,y){var z=3;return (x+y);}],[var z=3,return (x+y)])
-- >>> let (Right e3) = parse "x = 5"
-- >>> traverseExpression e3
-- ([],[])
-- >>> let (Right e4) = parse "x = function () {}"
-- >>> traverseExpression e4
-- ([function(){}],[])
-- >>> let (Right e5) = parse "function (x) {} + function (y) {}"
-- >>> traverseExpression e5
-- ([function(x){},function(y){}],[])
-- >>> let (Right e6) = parse "{ x: function (x) {}, y : function (y1, y2) {}, z: 5 }"
-- >>> traverseExpression e6
-- ([function(x){},function(y1,y2){}],[])
traverseExpression :: JSExpression -> TraverseTempResult
traverseExpression (JSRowVal func@(JSFunction _ body _ _)) =
  (func:moreF, moreS)
    where (moreF, moreS) = traverseStatements body
traverseExpression (JSAssignment expr1 expr2) =
  concatPair (traverseExpression expr1) (traverseExpression expr2)
traverseExpression (JSInfixOperate _ expr1 expr2) =
  concatPair (traverseExpression expr1) (traverseExpression expr2)
traverseExpression (JSSuffixUnaryOperate _ expr) =
  traverseExpression expr
traverseExpression (JSPrefixUnaryOperate _ expr) =
  traverseExpression expr
traverseExpression (JSFunctionCall func args) =
  concatPair (traverseExpression func) (traverseExpressions args)
traverseExpression (JSDotFieldGet expr1 expr2) =
  concatPair (traverseExpression expr1) (traverseExpression expr2)
traverseExpression (JSBracketFieldGet expr1 expr2) =
  concatPair (traverseExpression expr1) (traverseExpression expr2)
traverseExpression (JSObjectLiteral kvPairs) =
  traverseExpressions (snd <$> kvPairs)
traverseExpression (JSArrayLiteral exprs) =traverseExpressions exprs
traverseExpression _ = ([], [])

-- | traverse many expressions in a list
traverseExpressions :: [JSExpression] -> TraverseTempResult
traverseExpressions exprs = concatPairs $ traverseExpression <$> exprs

-- | traverse an statement to find all function declaration and statements
--
-- Examples:
-- >>> let parse = parseString jsStatementParser
-- >>> let (Right s1) = parse "3;"
-- >>> traverseStatement s1
-- ([],[3])
-- >>> let (Right s2) = parse "var x = 3;"
-- >>> traverseStatement s2
-- ([],[var x=3])
-- >>> let (Right s3) = parse "var x = function (x) { return x * 2; }"
-- >>> traverseStatement s3
-- ([function(x){return (x*2);}],[var x=function(x){return (x*2);},return (x*2)])
-- >>> let (Right s4) = parse "function f (x) { return x * 2; }"
-- >>> traverseStatement s4
-- ([function(x){return (x*2);}],[function f(x){return (x*2);},return (x*2)])
--
-- >>> :{
--  let str = "function f1(x) { function f2(y) {y ++; return (y + 1);} ; " ++
--            "var z = x + x; return (x + f(x))}"
-- :}
--
-- >>> let (Right s5) = parse str
-- >>> let (fs, ss) = traverseStatement s5
-- >>> (length fs, length ss)
-- (2,7)
--
-- >>> let str = "for (var x = 3; x <= 4; x ++) { var y = 5; break; }"
-- >>> let (Right s6) = parse str
-- >>> let (fs, ss) = traverseStatement s6
-- >>> (length fs, length ss)
-- (0,7)
traverseStatement  :: JSStatement -> TraverseTempResult
traverseStatement stmt = (contentF, stmt:contentS)
  where
    (contentF, contentS) = traverseContent (statementContnet stmt)
    traverseContent :: JSStatementContent -> TraverseTempResult
    traverseContent (JSRowExpression expr) = traverseExpression expr
    traverseContent (JSDeclaration _ (Just expr)) = traverseExpression expr
    traverseContent (JSFuncDeclaration _
                     func@JSFunction { functionBody = body }) =
      (func:moreF, moreS)
        where (moreF, moreS) = traverseStatements body
    traverseContent (JSIf cond true Nothing) =
      concatPairs [traverseExpression cond, traverseStatement true]
    traverseContent (JSIf cond true (Just false)) =
      concatPairs [ traverseExpression cond,
                    traverseStatement true,
                    traverseStatement false ]
    traverseContent (JSForLoop begin cond after body) =
      concatPairs [ traverseStatements begin
                  , traverseStatements cond
                  , traverseStatements after
                  , traverseStatement  body
                  ]
    traverseContent (JSBlock stmts) = traverseStatements stmts
    traverseContent _ = ([], [])

-- | traverse all statements in a list, then concat the result
--
-- Examples:
-- >>> let parse = parseString jsProgramParser
-- >>> let (Right p1) = parse " function f(x) { x ++; return (x + 1) ; } var x = 3; x++; "
-- >>> let (fs, ss) = traverseStatements p1
-- >>> (length fs, length ss)
-- (1,5)
-- >>> traverseStatements []
-- ([],[])
traverseStatements :: [JSStatement] -> TraverseTempResult
traverseStatements stmts = concatPairs $ traverseStatement <$> stmts

-- | the type of the statement, this is used to find out if the function should
-- return, or the next step of loop
-- if there is a invalid type (for example: try to return in the top level),
-- we need to know which statement cause the exception, so the break type,
-- continue type and the return type should remember the first statement that
-- cause the type
data StatementType = NormalStatement   JSVal
                   | BreakStatement    (Maybe JSStatement)
                   | ContinueStatement (Maybe JSStatement)
                   | ReturnStatement   (Maybe JSStatement) JSVal
  deriving (Show)

-- | convert a invalid statement type to the corresponding error
-- the normal statement type should not be a invalid type
invalidStatementType :: StatementType -> JSError
invalidStatementType (BreakStatement    stmt)   = InvalidStatement stmt
invalidStatementType (ContinueStatement stmt)   = InvalidStatement stmt
invalidStatementType (ReturnStatement   stmt _) = InvalidStatement stmt
invalidStatementType _ = Default "Unknown statement type error"

-- | if the statement type needs to remembers a statement, but it does not,
-- it means that the statment just been called cause this type, so it should
-- remember that statement, otherwise, it should just return the type itself
convertStatementType :: StatementType -> JSStatement -> StatementType
convertStatementType (BreakStatement    Nothing)     stmt =
  BreakStatement    (Just stmt)
convertStatementType (ContinueStatement Nothing)     stmt =
  ContinueStatement (Just stmt)
convertStatementType (ReturnStatement   Nothing val) stmt =
  ReturnStatement   (Just stmt) val
convertStatementType returnStatement                 _    =
  returnStatement

-- | analyze a js statement, modify the program flow record
--
-- Examples:
-- >>> let parse = parseString jsProgramParser
-- >>> let s = " function f(x) { return x; } var x = 3; "
-- >>> let (Right prog) = parse s
-- >>> :{
--  do record <- initProgramFlowRecord prog
--     env    <- initEnv []
--     _      <- runExceptT $ analyzeStatement record env (prog !! 1)
--     let (fs, ss) = record
--     printCountNums ss
-- :}
-- [0,0,1]
analyzeStatement :: ProgramFlowRecord
                 -> JSEnv
                 -> JSStatement
                 -> IOThrowsError StatementType
analyzeStatement record@(_, sRunCounts) env stmt =
  do countRef <- maybe (liftIO (newIORef 0)) return (lookup stmt sRunCounts)
     count    <- liftIO $ readIORef countRef
     _        <- liftIO $ writeIORef countRef (count + 1)
     analyzeContent record env (statementContnet stmt)

-- | analyze the content of statement
-- this is the essense of statement analyze
analyzeContent :: ProgramFlowRecord
               -> JSEnv
               -> JSStatementContent
               -> IOThrowsError StatementType
analyzeContent record env (JSRowExpression expr) =
  NormalStatement <$> analyzeExpression record env expr
analyzeContent record (scope:_) (JSDeclaration var Nothing) =
  const (NormalStatement JSUndefined) <$> defineVarInScope scope var JSUndefined
analyzeContent record env@(scope:_) (JSDeclaration var (Just expr)) =
  do val <- analyzeExpression record env expr
     _   <- defineVarInScope scope var val
     return $ NormalStatement JSUndefined
analyzeContent record env@(scope:_) (JSFuncDeclaration name func) =
  const (NormalStatement JSUndefined) <$>
  defineVarInScope scope name (JSClosure func env)
analyzeContent record env (JSIf condExpr true maybeFalse) =
  analyzeIf record env condExpr true maybeFalse
analyzeContent record env JSForLoop { beginLoop     = begin
                                    , condition     = cond
                                    , afterEachTime = after
                                    , forLoopBody   = body } =
  analyzeForLoop record env begin cond after body
analyzeContent _ _ JSBreak    = return $ BreakStatement Nothing
analyzeContent _ _ JSContinue = return $ ContinueStatement Nothing
analyzeContent record env (JSReturn maybeExpr) =
  case maybeExpr of
    Nothing   -> return $ ReturnStatement Nothing JSUndefined
    Just expr -> do val <- analyzeExpression record env expr
                    return $ ReturnStatement Nothing val
analyzeContent record env (JSBlock stmts) =
  do scope <- liftIO nullScope
     analyzeStatements record (scope:env) stmts
analyzeContent _ _ JSEmptyStatement = return $ NormalStatement JSUndefined

-- | analyze if statement
-- if the condition is true, then the 'then' block will run,
-- otherwise, if there is a 'else' block, the 'else' block will run
--
-- Examples:
-- >>> let parse = parseString jsProgramParser
-- >>> let s = "if(x == 5) { return 3; } else { return 4; }"
-- >>> let (Right prog) = parse s
-- >>> :{
--  do record <- initProgramFlowRecord prog
--     env    <- initEnv [("x", JSInt 6)]
--     _      <- runExceptT $ analyzeStatement record env (prog !! 0)
--     let (fs, ss) = record
--     printCountNums ss
-- :}
-- [1,0,0,1,1]
analyzeIf :: ProgramFlowRecord
          -> JSEnv
          -> JSExpression
          -> JSStatement
          -> Maybe JSStatement
          -> IOThrowsError StatementType
analyzeIf record env condExpr true maybeFalse =
  do (JSBool cond) <- analyzeExpression record env condExpr
     if cond
       then runStatement true
       else runFalse maybeFalse
    where
      runStatement stmt = do scope <- liftIO nullScope
                             sType <- analyzeStatement record (scope:env) stmt
                             return (convertStatementType sType stmt)
      runFalse Nothing     = return $ NormalStatement JSUndefined
      runFalse (Just stmt) = runStatement stmt

-- | analyze for loop
--
-- Examples:
-- >>> let parse = parseString jsProgramParser
-- >>> let s = "var x = 3;\nfor(;x <= 3;x++) {\nvar x = 5;\nvar y = 10;\n}"
-- >>> let (Right prog) = parse s
-- >>> :{
--  do record <- initProgramFlowRecord prog
--     env    <- initEnv []
--     _      <- runExceptT $ analyzeStatement record env (prog !! 0)
--     _      <- runExceptT $ analyzeStatement record env (prog !! 1)
--     let (fs, ss) = record
--     printCountNums ss
-- :}
-- [1,1,1,2,1,1,1,1]
--
-- >>> let parse = parseString jsProgramParser
-- >>> let s = "var x = 3;\nfor(;x < 3;x++) {\nvar x = 5;\nvar y = 10;\n}"
-- >>> let (Right prog) = parse s
-- >>> :{
--  do record <- initProgramFlowRecord prog
--     env    <- initEnv []
--     _      <- runExceptT $ analyzeStatement record env (prog !! 0)
--     _      <- runExceptT $ analyzeStatement record env (prog !! 1)
--     let (fs, ss) = record
--     printCountNums ss
-- :}
-- [1,1,1,1,0,0,0,0]
--
-- >>> let parse = parseString jsProgramParser
-- >>> let s = "var x = 3;\nfor(;x <= 4;x++) {\nvar x = 5;\nvar y = 10;\n}"
-- >>> let (Right prog) = parse s
-- >>> :{
--  do record <- initProgramFlowRecord prog
--     env    <- initEnv []
--     _      <- runExceptT $ analyzeStatement record env (prog !! 0)
--     _      <- runExceptT $ analyzeStatement record env (prog !! 1)
--     let (fs, ss) = record
--     printCountNums ss
-- :}
-- [1,1,1,3,2,2,2,2]
analyzeForLoop :: ProgramFlowRecord
               -> JSEnv
               -> [JSStatement]
               -> [JSStatement]
               -> [JSStatement]
               -> JSStatement
               -> IOThrowsError StatementType
analyzeForLoop record env begin cond after body =
  do scope <- liftIO nullScope
     let loopEnv = scope:env
     sType <- analyzeStatements record loopEnv begin
     case sType of
       NormalStatement _ -> analyzeForLoopAfterBegin
                            record loopEnv cond after body
       _ -> throwError $ invalidStatementType sType

-- | first run all statement in condition part, get a boolean value, if it is
-- true, run the loop body then the 'after each time' staements
-- each time running the body part, a new empty scope should be insert to the
-- head of environment
analyzeForLoopAfterBegin :: ProgramFlowRecord
                         -> JSEnv
                         -> [JSStatement]
                         -> [JSStatement]
                         -> JSStatement
                         -> IOThrowsError StatementType
analyzeForLoopAfterBegin record env cond after body =
  do condType <- analyzeLoopCondition record env cond
     case condType of
       NormalStatement val -> let (JSBool shouldLoop) = asJSBool val in
         if not shouldLoop
           then return $ NormalStatement JSUndefined
           else
             do scope <- liftIO nullScope
                let bodyEnv = scope:env
                sType <- analyzeForLoopBodyAndTail
                         record bodyEnv cond after body
                case sType of
                  BreakStatement  _   -> return $ NormalStatement JSUndefined
                  ReturnStatement _ _ -> return sType
                  _ -> analyzeForLoopAfterBegin record env cond after body
       _ -> throwError $ invalidStatementType condType
    where
      analyzeForLoopBodyAndTail record env cond after body =
        do scope    <- liftIO nullScope
           bodyType <- analyzeStatement record (scope:env) body
           case bodyType of
             NormalStatement _ -> analyzeStatements record env after
             _                 -> return $ convertStatementType bodyType body

-- | analyze the condition statements of a loop statementContnet,
-- it should not only return a statement type, but also return a boolean value
-- representing whether the loop should continue
analyzeLoopCondition :: ProgramFlowRecord
                     -> JSEnv
                     -> [JSStatement]
                     -> IOThrowsError StatementType
analyzeLoopCondition _ _ [] = return $ NormalStatement JSUndefined
analyzeLoopCondition record env [stmt] = analyzeStatement record env stmt
analyzeLoopCondition record env (stmt:stmts) =
  do sType <- analyzeStatement record env stmt
     case sType of
       NormalStatement _ -> analyzeLoopCondition record env stmts
       _                 -> return $ convertStatementType sType stmt

-- | analyze a sequence of statements
--
-- Examples:
-- >>> let parse = parseString jsProgramParser
-- >>> let s = "function f(x) { \nreturn x; } \nvar a = 3;\nreturn a;a = 5;"
-- >>> let (Right prog) = parse s
-- >>> :{
--  do record <- initProgramFlowRecord prog
--     env    <- initEnv []
--     _      <- runExceptT $ analyzeStatements record env prog
--     let (fs, ss) = record
--     printCountNums ss
-- :}
-- [1,0,1,1,0]
analyzeStatements :: ProgramFlowRecord
                  -> JSEnv
                  -> [JSStatement]
                  -> IOThrowsError StatementType
analyzeStatements record env [] = return $ NormalStatement JSUndefined
analyzeStatements record env [stmt] =
  do sType <- analyzeStatement record env stmt
     return $ convertStatementType sType stmt
analyzeStatements record env (stmt:stmts) =
  do sType <- analyzeStatement record env stmt
     case sType of
       NormalStatement _ -> analyzeStatements record env stmts
       _ -> return $ convertStatementType sType stmt

-- | analyze a expression, when the expression is a function call,
-- the program flow record will change because the count for function call and
-- the counts of statements in that function will raise
--
-- Examples:
-- >>> let parse = parseString jsProgramParser
-- >>> let s = "function add(x, y) { return (x + y); } add (3, 4);"
-- >>> let (Right prog) = parse s
-- >>> :{
--  do record <- initProgramFlowRecord prog
--     env    <- initEnv []
--     _      <- runExceptT $ analyzeStatements record env prog
--     let (fs, ss) = record
--     printCountNums fs
--     printCountNums ss
-- :}
-- [1]
-- [1,1,1]
--
-- >>> let s = "function add(x, y) { return (x + y); } add (3, 4, 5);"
-- >>> let (Right prog) = parse s
-- >>> :{
--  do record <- initProgramFlowRecord prog
--     env    <- initEnv []
--     result <- runExceptT $ analyzeStatements record env prog
--     let (fs, ss) = record
--     printCountNums fs
--     printCountNums ss
--     print result
-- :}
-- [1]
-- [1,0,1]
-- Left Expected 2 args: found values [3,4,5]
--
-- >>> let s = "var add = 5; add (3, 4, 5);"
-- >>> let (Right prog) = parse s
-- >>> :{
--  do record <- initProgramFlowRecord prog
--     env    <- initEnv []
--     result <- runExceptT $ analyzeStatements record env prog
--     let (fs, ss) = record
--     printCountNums fs
--     printCountNums ss
--     print result
-- :}
-- []
-- [1,1]
-- Left Try to call a non-function value: 5
--
-- >>> let s = "var x = 2; function f1() { var x = 3; function f2() { return x; } return f2;} x = 5; f1()();"
-- >>> let (Right prog) = parse s
-- >>> :{
--  do record <- initProgramFlowRecord prog
--     env    <- initEnv []
--     result <- runExceptT $ analyzeStatements record env prog
--     print result
-- :}
-- Right (NormalStatement 3)
analyzeExpression :: ProgramFlowRecord
                  -> JSEnv
                  -> JSExpression
                  -> IOThrowsError JSVal
analyzeExpression record env (JSFunctionCall closureExpr argExprs) =
  do closure <- analyzeExpression record env closureExpr
     analyzeFunctionCall record env closure argExprs
analyzeExpression record env expr = eval env expr

-- | analyze a function call, the input value has to be a js closure,
-- otherwise, a not function error will occur
-- a function call expression should be evaluate in the environment where it
-- has been defined, and the arguments should be evaluate in the environment
-- where the function is called
analyzeFunctionCall :: ProgramFlowRecord
                    -> JSEnv
                    -> JSVal
                    -> [JSExpression]
                    -> IOThrowsError JSVal
analyzeFunctionCall record@(fCount, _) env (JSClosure func funcEnv) argExprs =
  do countRef <- maybe (liftIO (newIORef 0)) return (lookup func fCount)
     count    <- liftIO $ readIORef countRef
     _        <- liftIO $ writeIORef countRef (count + 1)
     let analyzeExpr = analyzeExpression record env
     args     <- sequence (analyzeExpr <$> argExprs)
     let paramNum = length $ functionParams func
     if length args /= paramNum
       then throwError $ NumArgs (toInteger paramNum) args
       else
         do scope <- liftIO . initScope $ zip (functionParams func) args
            sType <- analyzeStatements record (scope:funcEnv) (functionBody func)
            case sType of
              ReturnStatement _ val -> return val
              NormalStatement _     -> return JSUndefined
              _ -> throwError $ invalidStatementType sType
analyzeFunctionCall _ _ notFunc _ = throwError $ NotFunction notFunc
