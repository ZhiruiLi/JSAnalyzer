module JSEvaluator
( eval
, nullEnv
, nullScope
, bindVars
, readNamesInScope
, readNamesInEnv
, initEnv
, initScope
, isBound
, isBoundInScope
, appendScope
, getVar
, getVarInScope
, setVar
, setVarInScope
, defineVarInScope
, asJSBool
, asJSNumber
, asJSString
) where

import           Data.IORef
import           Data.Maybe (isJust)
import           JSEntity
import           JSError

-- | empty environment
nullEnv :: IO JSEnv
nullEnv  = return []

-- | empty scope
nullScope :: IO JSScope
nullScope  = newIORef []

-- | bind name-value pairs to the scope
--
-- Examples:
-- >>> :{
--  do initScope <- nullScope
--     newScope  <- bindVars initScope [("x", JSNull), ("y", JSNull)]
--     names     <- readNamesInScope newScope
--     print names
-- :}
-- ["x","y"]
bindVars :: JSScope -> [(String, JSVal)] -> IO JSScope
bindVars scopeRef bindings =
  do oldScope <- readIORef scopeRef
     newScope <- extendScope bindings oldScope
     newIORef newScope
     where
       extendScope :: [(s, a)] -> [(s, IORef a)] -> IO [(s, IORef a)]
       extendScope bindings scope = fmap (++ scope) (mapM addBinding bindings)
       addBinding :: (s, a) -> IO (s, IORef a)
       addBinding (name, value) = do ref <- newIORef value
                                     return (name, ref)

-- | read all names in a environment, return a nested list of strings
-- each list represent a scope
readNamesInEnv :: JSEnv -> IO [[String]]
readNamesInEnv envRef = sequence (readNamesInScope <$> envRef)

-- | read all variables' name of bindings in a scope
readNamesInScope :: JSScope -> IO [String]
readNamesInScope scopeRef = do s <- readIORef scopeRef
                               return $ fst <$> s

-- | determine whether a variable is bound in a given scope
--
-- Examples:
-- >>> :{
--  do empty <- nullScope
--     b     <- isBoundInScope empty "x"
--     print b
-- :}
-- False
--
-- >>> :{
--  do s <- initScope [("x", JSNull), ("y", JSNull)]
--     b <- isBoundInScope s "y"
--     print b
-- :}
-- True
isBoundInScope :: JSScope -> String -> IO Bool
isBoundInScope sr var = fmap (isJust . lookup var) (readIORef sr)

-- | determine whether a variable is bound in a given environment
--
-- Examples:
-- >>> :{
--  do b <- isBound [] "x"
--     print b
-- :}
-- False
--
-- >>> :{
--  do initScope <- nullScope
--     newScope  <- bindVars initScope [("x", JSNull), ("y", JSNull)]
--     b         <- isBound [newScope] "y"
--     print b
-- :}
-- True
isBound :: JSEnv -> String -> IO Bool
isBound []                   _   = return False
isBound (scopeRef:nestedEnv) var =
  do b <- isBoundInScope scopeRef var
     if b then return b else isBound nestedEnv var

-- | append a scope to an environment
--
-- Examples:
-- >>> :{
--  do env   <- appendScope nullEnv nullScope
--     names <- readNamesInEnv env
--     print names
-- :}
-- [[]]
--
-- >>> :{
--  do initScope <- nullScope
--     env       <- appendScope nullEnv
--                    (bindVars initScope [("x", JSNull), ("y", JSNull)])
--     newEnv    <- appendScope (return env) nullScope
--     names     <- readNamesInEnv newEnv
--     print names
-- :}
-- [[],["x","y"]]
appendScope :: IO JSEnv -> IO JSScope -> IO JSEnv
appendScope ioEnv ioScope = do env   <- ioEnv
                               scope <- ioScope
                               return (scope : env)

-- | init a scope with some bindings
--
-- Examples:
-- >>> :{
--  do s     <- initScope [("x", JSNull), ("y", JSNull)]
--     names <- readNamesInScope s
--     print names
-- :}
-- ["x","y"]
initScope :: [(String, JSVal)] -> IO JSScope
initScope bindings = do empty <- nullScope
                        bindVars empty bindings

-- | init a environment with a single scope with some bindings
--
-- Examples:
-- >>> :{
--  do env   <- initEnv [("x", JSNull), ("y", JSNull)]
--     names <- readNamesInEnv env
--     print names
-- :}
-- [["x","y"]]
initEnv :: [(String, JSVal)] -> IO JSEnv
initEnv bindings = appendScope nullEnv initScope
  where initScope = do empty <- nullScope
                       bindVars empty bindings

-- | find and get the value of a variable in a specific scope
-- if the variable is not in the scope an UnboundVar error will occur
--
-- Examples:
-- >>> :{
--  do empty  <- nullScope
--     result <- runExceptT $ getVarInScope empty "x"
--     print result
-- :}
-- Left Try to get an unbound variable: x
--
-- >>> :{
--  do empty  <- nullScope
--     new    <- bindVars empty [("x", JSNull), ("y", JSInt 3), ("z", JSNull)]
--     result <- runExceptT $ getVarInScope new "y"
--     print result
-- :}
-- Right 3
getVarInScope :: JSScope -> String -> IOThrowsError JSVal
getVarInScope scopeRef var =
  do scope <- liftIO $ readIORef scopeRef
     maybe (throwError $ UnboundVar "Try to get an unbound variable" var)
           (liftIO . readIORef) (lookup var scope)

-- | find and get the value of a variable in a specific environment,
-- if there are more than one variables have the same name,
-- the variable in the outer scope will be found.
-- if the variable is not in the environment an UnboundVar error will occur
--
-- Examples:
-- >>> :{
--  do result <- runExceptT $ getVar [] "x"
--     print result
-- :}
-- Left Try to get an unbound variable: x
--
-- >>> :{
--  do emptyS <- nullScope
--     env    <- initEnv [("x", JSNull), ("y", JSInt 5), ("z", JSNull)]
--     result <- runExceptT $ getVar (emptyS:env) "y"
--     print result
-- :}
-- Right 5
getVar :: JSEnv -> String -> IOThrowsError JSVal
getVar []         var =
  throwError $ UnboundVar "Try to get an unbound variable" var
getVar (sRef:env) var =
  do result <- liftIO $ runExceptT (getVarInScope sRef var)
     case result of
       Left  _ -> getVar env var
       Right x -> return x

-- | set a variable to a specific value in the scope,
-- if the variable is not in the scope,
-- an UnboundVar error will occur
--
-- Examples:
-- >>> :{
--  do emptyS <- nullScope
--     newS   <- bindVars emptyS [("x", JSNull), ("y", JSNull), ("z", JSNull)]
--     res1   <- runExceptT $ setVarInScope newS "y" (JSInt 1)
--     res2   <- runExceptT $ getVarInScope newS "y"
--     print (res1, res2)
-- :}
-- (Right 1,Right 1)
--
-- >>> :{
--  do emptyS <- nullScope
--     result <- runExceptT $ setVarInScope emptyS "x" JSNull
--     print result
-- :}
-- Left Try to set an unbound variable: x
setVarInScope :: JSScope -> String -> JSVal -> IOThrowsError JSVal
setVarInScope scopeRef var value =
  do scope <- liftIO $ readIORef scopeRef
     maybe (throwError $ UnboundVar "Try to set an unbound variable" var)
           (liftIO . (`writeIORef` value))
           (lookup var scope)
     return value

-- | find and set a variable to a specific value in given environment,
-- if there are more than one variables have the same name,
-- the value of variable in the outer scope will be setted.
-- if the variable is not in the environment an UnboundVar error will occur
--
-- Examples:
-- >>> :{
--  do result <- runExceptT $ setVar [] "x" JSNull
--     print result
-- :}
-- Left Try to set an unbound variable: x
--
-- >>> :{
--  do emptyS <- nullScope
--     env    <- initEnv [("x", JSNull), ("y", JSNull), ("z", JSNull)]
--     res1   <- runExceptT $ setVar (emptyS:env) "y" (JSInt 3)
--     res2   <- runExceptT $ getVar (emptyS:env) "y"
--     print (res1, res2)
-- :}
-- (Right 3,Right 3)
setVar :: JSEnv -> String -> JSVal -> IOThrowsError JSVal
setVar []         var _     =
  throwError $ UnboundVar "Try to set an unbound variable" var
setVar (sRef:env) var value =
  do result <- liftIO $ runExceptT (setVarInScope sRef var value)
     case result of
       Left  _ -> setVar env var value
       Right x -> return x

-- | define a variable in the given scope,
-- if a variable is already in the scope, a NameConflit error will occur
--
-- Examples:
-- >>> :{
--  do s   <- initScope [("x", JSNull)]
--     res <- runExceptT $ defineVarInScope s "x" JSNull
--     print res
-- :}
-- Left Name conflict: x has already been defined
--
-- >>> :{
--  do s             <- initScope [("y", JSNull)]
--     isBoundBefore <- isBoundInScope s "x"
--     boundRes      <- runExceptT $ defineVarInScope s "x" JSNull
--     isBoundAfter  <- isBoundInScope s "x"
--     print (isBoundBefore,boundRes,isBoundAfter)
-- :}
-- (False,Right null,True)
defineVarInScope :: JSScope -> String -> JSVal -> IOThrowsError JSVal
defineVarInScope scopeRef var value =
  do alreadyDefined <- liftIO $ isBoundInScope scopeRef var
     if alreadyDefined
       then throwError $ NameConflict var
       else liftIO $ do
         valueRef <- newIORef value
         scope    <- readIORef scopeRef
         writeIORef scopeRef ((var, valueRef):scope)
         return value

-- | evaluate a js expression in a specific environment,
-- every evaluation will return a result value,
-- and it may update the environment
--
-- Examples:
-- >>> runExceptT $ eval [] (JSRowVal (JSString "abc"))
-- Right abc
-- >>> runExceptT $ eval [] (JSInfixOperate JSEqualTo (JSRowVal (JSInt 3)) (JSRowVal (JSInt 10)))
-- Right false
-- >>> runExceptT $ eval [] (JSInfixOperate JSEqualTo (JSRowVal (JSInt 3)) (JSRowVal (JSInt 3)))
-- Right true
-- >>> runExceptT $ eval [] (JSInfixOperate JSEqualTo (JSRowVal (JSInt 3)) (JSRowVal (JSString "3")))
-- Right true
-- >>> :{
--  do env <- initEnv [("x", JSNull)]
--     val <- runExceptT $ eval env (JSAssignment (JSRowVal (JSAtom "x")) (JSRowVal (JSInt 1)))
--     res <- runExceptT $ eval env (JSInfixOperate JSAddition (JSRowVal (JSAtom "x")) (JSRowVal (JSInt 1)))
--     print res
-- :}
-- Right 2
--
-- !!! Here is a bug: "a[3] = 5" will fail, this bug occurs because the "eval"
-- function do not know how to evaluate a non-atom expression into a value
-- reference.
eval :: JSEnv -> JSExpression -> IOThrowsError JSVal
eval env (JSRowVal (JSAtom var)) = getVar env var
eval env (JSRowVal func@JSFunction{}) = return (JSClosure func env)
eval _   (JSRowVal v) = return v
eval env (JSAssignment varExpr valueExpr) =
  do value <- eval env valueExpr
     case varExpr of
       JSRowVal (JSAtom name) -> setVar env name value
       _                      -> throwError $
         BadSpecialForm "Unrecognized special form of assignment" varExpr
eval env (JSInfixOperate op expr1 expr2) =
  evalInfixExpression env op expr1 expr2
eval env (JSSuffixUnaryOperate op expr) =
  evalSuffixExpression env op expr
eval env (JSPrefixUnaryOperate op expr) =
  evalPrefixExpression env op expr
eval env (JSDotFieldGet obj field) =
  evalDotFieldGet env obj field
eval env (JSBracketFieldGet obj field) =
  evalBracketFiledGet env obj field
eval env (JSObjectLiteral pairs) =
  evalObjectLiteral env pairs
eval env (JSArrayLiteral elems) =
  evalArrayLiteral env elems

-- | convert a string to an instance of class Read a
-- if succeed, the result will be encapsulate in Just constructor,
-- otherwise, it will return Nothing
--
-- Examples:
-- >>> :{
--  let x :: Maybe Double
--      x = readMay "123.456"
-- :}
--
-- >>> x
-- Just 123.456
--
-- >>> :{
--  let x :: Maybe Double
--      x = readMay "123"
-- :}
--
-- >>> x
-- Just 123.0
--
-- >>> :{
--  let x :: Maybe Double
--      x = readMay "123abc"
-- :}
--
-- >>> x
-- Nothing
--
-- >>> :{
--  let x :: Maybe Int
--      x = readMay "123.456"
-- :}
--
-- >>> x
-- Nothing
readMay :: Read a => String -> Maybe a
readMay s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

-- | convert a js val to a js number,
-- a valid js number is either a js int or a js float,
-- a invalid js number is NaN
--
-- Examples:
-- >>> asJSNumber (JSString "1.2")
-- 1.2
-- >>> asJSNumber (JSString "abc")
-- NaN
-- >>> asJSNumber (JSBool True)
-- 1
-- >>> asJSNumber (JSBool False)
-- 0
asJSNumber :: JSVal -> JSVal
asJSNumber val@(JSInt x)      = val
asJSNumber val@(JSFloat x)    = val
asJSNumber     (JSBool True)  = JSInt 1
asJSNumber     (JSBool False) = JSInt 0
asJSNumber     (JSString s)   =
  case readIntMay s of
    Just x  -> JSInt x
    Nothing -> case readFloatMay s of
                 Just x  -> JSFloat x
                 Nothing -> JSNaN
  where
    readIntMay   :: String -> Maybe Integer
    readIntMay    = readMay
    readFloatMay :: String -> Maybe Double
    readFloatMay  = readMay
asJSNumber _                  = JSNaN

-- | convert a js val to a js bool,
--
-- Examples:
-- >>> asJSBool (JSString "1.2")
-- true
-- >>> asJSBool JSNaN
-- false
asJSBool :: JSVal -> JSVal
asJSBool = JSBool . asBool
  where
    asBool :: JSVal -> Bool
    asBool JSUndefined   = False
    asBool JSNull        = False
    asBool JSNaN         = False
    asBool (JSInt 0)     = False
    asBool (JSFloat 0)   = False
    asBool (JSString "") = False
    asBool (JSBool b)    = b
    asBool _             = True

-- | convert a js val to a js string
--
-- Examples:
-- >>> asJSString (JSInt 1)
-- 1
asJSString :: JSVal -> JSVal
asJSString val@(JSString _) = val
asJSString val = JSString (show val)

-- | the not operation of js value,
-- if the js value is not a js bool value,
-- it will be converted to a js bool value then call jsLogicNot again
jsLogicNot :: JSVal -> IOThrowsError JSVal
jsLogicNot (JSBool b) = return . JSBool $ not b
jsLogicNot val        = jsLogicNot $ asJSBool val

-- | operation of infix operator
type JSInfixOperation = JSVal -> JSVal -> IOThrowsError JSVal

-- | js addition, add any two js numbers together will behave as math addition,
-- that means if any valid number plus a NaN, the result will be NaN.
-- otherwise, it will behave as string appending converting both input values
-- to string values
jsAdd :: JSInfixOperation
jsAdd (JSInt x)   (JSInt y)   = return . JSInt   $ x + y
jsAdd (JSFloat x) (JSFloat y) = return . JSFloat $ x + y
jsAdd (JSFloat x) (JSInt y)   = return . JSFloat $ x + fromInteger y
jsAdd (JSInt x)   (JSFloat y) = return . JSFloat $ fromInteger x + y
jsAdd JSNaN       (JSInt _)   = return JSNaN
jsAdd (JSInt _)   JSNaN       = return JSNaN
jsAdd JSNaN       (JSFloat _) = return JSNaN
jsAdd (JSFloat _) JSNaN       = return JSNaN
jsAdd val1 val2 = return $ JSString (show val1 `mappend` show val2)

-- | js substraction,
-- it will try to convert the two input values to js numbers,
-- and then substract the two js numbers as math substraction
jsSubtract :: JSInfixOperation
jsSubtract (JSInt x) (JSInt y)     = return . JSInt   $ x - y
jsSubtract (JSFloat x) (JSFloat y) = return . JSFloat $ x - y
jsSubtract (JSFloat x) (JSInt y)   = return . JSFloat $ x - fromInteger y
jsSubtract (JSInt x) (JSFloat y)   = return . JSFloat $ fromInteger x - y
jsSubtract JSNaN _                 = return JSNaN
jsSubtract _ JSNaN                 = return JSNaN
jsSubtract val1 val2 = jsSubtract (asJSNumber val1) (asJSNumber val2)

-- | js multiplication,
-- it will try to convert the two input values to js numbers,
-- and then multiply the two js numbers as math multiplication
jsMultiply :: JSInfixOperation
jsMultiply (JSInt x)   (JSInt y)   = return . JSInt   $ x * y
jsMultiply (JSFloat x) (JSFloat y) = return . JSFloat $ x * y
jsMultiply (JSFloat x) (JSInt y)   = return . JSFloat $ x * fromInteger y
jsMultiply (JSInt x)   (JSFloat y) = return . JSFloat $ fromInteger x * y
jsMultiply JSNaN       _           = return JSNaN
jsMultiply _           JSNaN       = return JSNaN
jsMultiply val1 val2 = jsMultiply (asJSNumber val1) (asJSNumber val2)

-- | js division,
-- it will try to convert the two input values to js numbers,
-- and then divide the two js numbers as math divide
jsDivide :: JSInfixOperation
jsDivide JSNaN       _            = return JSNaN
jsDivide _           JSNaN        = return JSNaN
jsDivide (JSInt x)   (JSInt 0)    = throwError $ Default "Devided by 0!"
jsDivide (JSFloat x) (JSFloat 0)  = throwError $ Default "Devided by 0!"
jsDivide (JSFloat x) (JSInt 0)    = throwError $ Default "Devided by 0!"
jsDivide (JSInt x)   (JSFloat 0)  = throwError $ Default "Devided by 0!"
jsDivide (JSInt x)   (JSInt y)    = return . JSFloat $ fromInteger x / fromInteger y
jsDivide (JSFloat x) (JSFloat y)  = return . JSFloat $ x / y
jsDivide (JSFloat x) (JSInt y)    = return . JSFloat $ x / fromInteger y
jsDivide (JSInt x)   (JSFloat y)  = return . JSFloat $ fromInteger x / y
jsDivide val1 val2 = jsDivide (asJSNumber val1) (asJSNumber val2)

-- | js modulus,
-- any js value will be convert to js numbers and then do the modulus calculate.
-- NaN do modulus calculation with any value will result NaN;
-- two integers do modulus calculation will result integer;
-- two floating point numbers do modulus calculation will result a floating
-- point number; one floating point number and one integer do modulus
-- calculation will result a floating point number
-- the result of (a % b) should has value between 0 and (abs b),
-- and it should have the same symbol as a
--
-- Examples:
-- >>> runExceptT $ jsMod JSNaN (JSInt 3)
-- Right NaN
-- >>> runExceptT $ jsMod (JSInt 7) (JSInt 3)
-- Right 1
-- >>> runExceptT $ jsMod (JSFloat 8.25) (JSInt 3)
-- Right 2.25
-- >>> runExceptT $ jsMod (JSFloat (-8.25)) (JSInt 3)
-- Right -2.25
-- >>> runExceptT $ jsMod (JSFloat (-8.25)) (JSInt (-3))
-- Right -2.25
-- >>> runExceptT $ jsMod (JSString "-8.25") (JSInt (-3))
-- Right -2.25
-- >>> runExceptT $ jsMod (JSString "abc") (JSInt 3)
-- Right NaN
jsMod :: JSInfixOperation
jsMod (JSInt _)        (JSInt y)        | y == 0    = return JSNaN
jsMod val1@(JSInt _)   (JSInt y)        | y <  0    = jsMod val1 (JSInt (-y))
jsMod val1@(JSInt x)   (JSInt y)        | abs x < y = return val1
jsMod (JSInt x)        val2@(JSInt y)   | x < 0     = jsMod (JSInt (x + y)) val2
jsMod (JSInt x)        val2@(JSInt y)   | x > 0     = jsMod (JSInt (x - y)) val2
jsMod (JSFloat _)      (JSFloat y)      | y == 0    = return JSNaN
jsMod val1@(JSFloat _) (JSFloat y)      | y <  0    = jsMod val1 (JSFloat (-y))
jsMod val1@(JSFloat x) (JSFloat y)      | abs x < y = return val1
jsMod (JSFloat x)      val2@(JSFloat y) | x < 0     = jsMod (JSFloat (x + y)) val2
jsMod (JSFloat x)      val2@(JSFloat y) | x > 0     = jsMod (JSFloat (x - y)) val2
jsMod val1@(JSFloat _) (JSInt y)                    = jsMod val1 (JSFloat (fromInteger y))
jsMod (JSInt x)        val2@(JSFloat _)             = jsMod (JSFloat (fromInteger x)) val2
jsMod JSNaN            _                            = return JSNaN
jsMod _                JSNaN                        = return JSNaN
jsMod val1@(JSInt _)   val2                         = jsMod val1 (asJSNumber val2)
jsMod val1             val2@(JSInt _)               = jsMod (asJSNumber val1) val2
jsMod val1@(JSFloat _) val2                         = jsMod val1 (asJSNumber val2)
jsMod val1             val2@(JSFloat _)             = jsMod (asJSNumber val1) val2
jsMod val1             val2                         = jsMod (asJSNumber val1) (asJSNumber val2)

-- | js equal behaves strangely, see the examples
--
-- Examples:
-- >>> runExceptT $ jsEqualTo JSNull JSUndefined
-- Right true
-- >>> runExceptT $ jsEqualTo (JSString "NaN") JSNaN
-- Right false
-- >>> runExceptT $ jsEqualTo (JSInt 5) JSNaN
-- Right false
-- >>> runExceptT $ jsEqualTo JSNaN JSNaN
-- Right false
-- >>> runExceptT $ jsEqualTo (JSBool False) (JSInt 0)
-- Right true
-- >>> runExceptT $ jsEqualTo (JSBool True) (JSInt 1)
-- Right true
-- >>> runExceptT $ jsEqualTo (JSBool True) (JSInt 2)
-- Right false
-- >>> runExceptT $ jsEqualTo JSUndefined (JSInt 0)
-- Right false
-- >>> runExceptT $ jsEqualTo JSNull (JSInt 0)
-- Right false
-- >>> runExceptT $ jsEqualTo (JSString "5") (JSInt 5)
-- Right true
jsEqualTo :: JSInfixOperation
jsEqualTo JSNull         JSUndefined    = return $ JSBool True
jsEqualTo JSUndefined    JSNull         = return $ JSBool True
jsEqualTo JSNaN          _              = return $ JSBool False
jsEqualTo _              JSNaN          = return $ JSBool False
jsEqualTo (JSBool True)  (JSInt 1)      = return $ JSBool True
jsEqualTo (JSInt 1)      (JSBool True)  = return $ JSBool True
jsEqualTo (JSBool False) (JSInt 0)      = return $ JSBool True
jsEqualTo (JSInt 0)      (JSBool False) = return $ JSBool True
jsEqualTo (JSInt x)      (JSInt y)      = return $ JSBool (x == y)
jsEqualTo (JSFloat x)    (JSFloat y)    = return $ JSBool (x == y)
jsEqualTo (JSFloat x)    (JSInt y)      = return $ JSBool (x == fromInteger y)
jsEqualTo (JSInt x)      (JSFloat y)    = return $ JSBool (fromInteger x == y)
jsEqualTo val1 val2 = return $ JSBool (show val1 == show val2)

-- | the NOT operation of jsEqualTo
jsNotEqual :: JSInfixOperation
jsNotEqual val1 val2 =
  do val <- jsEqualTo val1 val2
     jsLogicNot val

-- | determine if the first js value is greater then the second one,
-- for js numbers, it will compare the values,
-- for NaN, all comparation will be false,
-- otherwise, it will compare the lexicographic order
-- converting the two values to js string
jsGreaterThan :: JSInfixOperation
jsGreaterThan (JSInt x)        (JSInt y)        = return . JSBool $ x > y
jsGreaterThan (JSFloat x)      (JSFloat y)      = return . JSBool $ x > y
jsGreaterThan (JSFloat x)      (JSInt y)        = return . JSBool $ x > fromInteger y
jsGreaterThan (JSInt x)        (JSFloat y)      = return . JSBool $ fromInteger x > y
jsGreaterThan JSNaN            _                = return . JSBool $ False
jsGreaterThan _                JSNaN            = return . JSBool $ False
jsGreaterThan val1@(JSFloat _) val2             =
  jsGreaterThan (asJSNumber val1) (asJSNumber val2)
jsGreaterThan val1             val2@(JSFloat _) =
  jsGreaterThan (asJSNumber val1) (asJSNumber val2)
jsGreaterThan val1@(JSInt _)   val2             =
  jsGreaterThan (asJSNumber val1) (asJSNumber val2)
jsGreaterThan val1             val2@(JSInt _)   =
  jsGreaterThan (asJSNumber val1) (asJSNumber val2)
jsGreaterThan val1             val2             =
  return . JSBool $ show val1 > show val2

-- | determine if the first js value is less then the second one,
-- for js numbers, it will compare the values,
-- for NaN, all comparation will be false,
-- otherwise, it will compare the lexicographic order
-- converting the two values to js string
jsLessThan :: JSInfixOperation
jsLessThan (JSInt x)        (JSInt y)        = return . JSBool $ x < y
jsLessThan (JSFloat x)      (JSFloat y)      = return . JSBool $ x < y
jsLessThan (JSFloat x)      (JSInt y)        = return . JSBool $ x < fromInteger y
jsLessThan (JSInt x)        (JSFloat y)      = return . JSBool $ fromInteger x < y
jsLessThan JSNaN            _                = return . JSBool $ False
jsLessThan _                JSNaN            = return . JSBool $ False
jsLessThan val1@(JSFloat _) val2             =
  jsLessThan (asJSNumber val1) (asJSNumber val2)
jsLessThan val1             val2@(JSFloat _) =
  jsLessThan (asJSNumber val1) (asJSNumber val2)
jsLessThan val1@(JSInt _)   val2             =
  jsLessThan (asJSNumber val1) (asJSNumber val2)
jsLessThan val1             val2@(JSInt _)   =
  jsLessThan (asJSNumber val1) (asJSNumber val2)
jsLessThan val1             val2             =
  return . JSBool $ show val1 < show val2

-- | determine if the first js value is greater or equal to the second one,
-- using jsGreaterThan and jsEqualTo
jsGreaterOrEqual :: JSInfixOperation
jsGreaterOrEqual val1 val2 =
  do (JSBool b1) <- jsGreaterThan val1 val2
     (JSBool b2) <- jsEqualTo val1 val2
     return . JSBool $ b1 || b2

-- | determine if the first js value is less or equal to the second one,
-- using jsLessThan and jsEqualTo
jsLessOrEqual :: JSInfixOperation
jsLessOrEqual val1 val2 =
  do (JSBool b1) <- jsLessThan val1 val2
     (JSBool b2) <- jsEqualTo val1 val2
     return . JSBool $ b1 || b2

-- | compare two js values,
-- the function will return true if and only the two input values
-- have the same value and the same type
-- but comparation of two NaN will always be false
jsEqualValAndType :: JSInfixOperation
jsEqualValAndType val1 val2 = return . JSBool $ val1 == val2

-- | the NOT operation of jsEqualValAndType
jsNotEqualValOrType :: JSInfixOperation
jsNotEqualValOrType val1 val2 =
  do val <- jsEqualValAndType val1 val2
     jsLogicNot val

-- | mapping from js pure infix operators to operations
jsPrimaryInfixOperators :: [(JSOperator, JSInfixOperation)]
jsPrimaryInfixOperators  =
  [ (JSAddition,          jsAdd)               -- +
  , (JSSubtraction,       jsSubtract)          -- -
  , (JSMultiplication,    jsMultiply)          -- \*
  , (JSDivision,          jsDivide)            -- /
  , (JSModulus,           jsMod)               -- %
  , (JSEqualTo,           jsEqualTo)           -- ==
  , (JSNotEqual,          jsNotEqual)          -- !=
  , (JSEqualValAndType,   jsEqualValAndType)   -- ===
  , (JSNotEqualValOrType, jsNotEqualValOrType) -- !==
  , (JSGreaterThan,       jsGreaterThan)       -- >
  , (JSLessThan,          jsLessThan)          -- <
  , (JSGreaterOrEqual,    jsGreaterOrEqual)    -- >=
  , (JSLessOrEqual,       jsLessOrEqual)       -- <=
  ]

-- | evaluate expression with primary infix operator
--
-- Examples:
-- >>> let x = JSRowVal (JSString "3.3")
-- >>> let y = JSRowVal (JSInt 4)
-- >>> runExceptT $ evalInfixExpression [] JSAddition x y
-- Right 3.34
-- >>> let x = JSRowVal (JSFloat 1.25)
-- >>> let y = JSRowVal (JSInt 4)
-- >>> runExceptT $ evalInfixExpression [] JSAddition x y
-- Right 5.25
-- >>> let x = JSRowVal (JSString "1.75")
-- >>> let y = JSRowVal (JSInt 1)
-- >>> runExceptT $ evalInfixExpression [] JSSubtraction x y
-- Right 0.75
-- >>> let x = JSRowVal (JSInt 3)
-- >>> let y = JSRowVal (JSString "3")
-- >>> runExceptT $ evalInfixExpression [] JSEqualTo x y
-- Right true
-- >>> let x = JSRowVal (JSInt 3)
-- >>> let y = JSRowVal (JSFloat 2.5)
-- >>> runExceptT $ evalInfixExpression [] JSEqualTo x y
-- Right false
-- >>> let x = JSRowVal (JSBool False)
-- >>> let y = JSRowVal (JSInt 5)
-- >>> runExceptT $ evalInfixExpression [] JSLogicAnd x y
-- Right false
-- >>> let x = JSRowVal (JSBool False)
-- >>> let y = JSRowVal (JSInt 5)
-- >>> runExceptT $ evalInfixExpression [] JSLogicOr x y
-- Right true
-- >>> let x = JSRowVal (JSString "6.3")
-- >>> let y = JSRowVal (JSInt 5)
-- >>> runExceptT $ evalInfixExpression [] JSGreaterThan x y
-- Right true
-- >>> let x = JSRowVal (JSString "6.3")
-- >>> let y = JSRowVal (JSInt 5)
-- >>> runExceptT $ evalInfixExpression [] JSLessThan x y
-- Right false
-- >>> let x = JSRowVal (JSInt 5)
-- >>> let y = JSRowVal (JSString "5")
-- >>> runExceptT $ evalInfixExpression [] JSLessOrEqual x y
-- Right true
-- >>> let x = JSRowVal (JSInt 5)
-- >>> let y = JSRowVal (JSString "5")
-- >>> runExceptT $ evalInfixExpression [] JSNotEqualValOrType x y
-- Right true
-- >>> let x = JSRowVal (JSInt 5)
-- >>> let y = JSRowVal (JSFloat 5)
-- >>> runExceptT $ evalInfixExpression [] JSEqualValAndType x y
-- Right true
evalInfixExpression :: JSEnv
                    -> JSOperator
                    -> JSExpression
                    -> JSExpression
                    -> IOThrowsError JSVal
evalInfixExpression env JSLogicAnd expr1 expr2 =
  do val1 <- eval env expr1
     case asJSBool val1 of
       JSBool False -> return $ JSBool False
       JSBool True  -> do val2 <- eval env expr2
                          return $ asJSBool val2
evalInfixExpression env JSLogicOr expr1 expr2 =
  do val1 <- eval env expr1
     case asJSBool val1 of
       JSBool True  -> return $ JSBool True
       JSBool False -> do val2 <- eval env expr2
                          return $ asJSBool val2
evalInfixExpression env op expr1 expr2 =
  maybe (throwError . Default $ show op `mappend` " is not an infix operator")
        useOperator
        (lookup op jsPrimaryInfixOperators)
    where
      useOperator :: JSInfixOperation -> IOThrowsError JSVal
      useOperator f = do val1 <- eval env expr1
                         val2 <- eval env expr2
                         f val1 val2

-- | strict mode of math calculate,
-- only when both input values are js numbers the operator will be applied
--
-- Examples:
-- >>> runExceptT $ jsStrictMathOperate (JSFloat 1.25) (JSInt 1) jsAdd
-- Right 2.25
-- >>> runExceptT $ jsStrictMathOperate (JSString "1") (JSInt 1) jsAdd
-- Left Invalid type: expected Number, found 1
jsStrictMathOperate :: JSVal -> JSVal -> JSInfixOperation -> IOThrowsError JSVal
jsStrictMathOperate val1@(JSInt _)    val2@(JSInt _)   op =
  op val1 val2
jsStrictMathOperate val1@(JSFloat _)  val2@(JSFloat _) op =
  op val1 val2
jsStrictMathOperate (JSInt x)         val2@(JSFloat _) op =
  op (JSFloat $ fromInteger x) val2
jsStrictMathOperate val1@(JSFloat _)  (JSInt y)        op =
  op val1 (JSFloat $ fromInteger y)
jsStrictMathOperate (JSInt _)         val2             _  =
  throwError $ TypeMismatch "Number" val2
jsStrictMathOperate (JSFloat _)         val2           _  =
  throwError $ TypeMismatch "Number" val2
jsStrictMathOperate val1             _                 _  =
  throwError $ TypeMismatch "Number" val1

jsOne :: JSVal
jsOne  = JSInt 1

-- | evaluate an expression with a prefix operator
--
-- Examples:
-- >>> runExceptT $ evalPrefixExpression [] JSLogicNot (JSRowVal (JSBool True))
-- Right false
-- >>> runExceptT $ evalPrefixExpression [] JSLogicNot (JSRowVal JSNull)
-- Right true
-- >>> runExceptT $ evalPrefixExpression [] JSIncrement (JSRowVal (JSAtom "x"))
-- Left Try to get an unbound variable: x
--
-- >>> :{
--  do env    <- initEnv [("x", JSInt 5)]
--     result <- runExceptT $ evalPrefixExpression env JSIncrement (JSRowVal (JSAtom "x"))
--     val    <- runExceptT $ getVar env "x"
--     print (result, val)
-- :}
-- (Right 6,Right 6)
--
-- >>> :{
--  do env    <- initEnv [("x", JSInt 5), ("y", JSFloat 3.25)]
--     result <- runExceptT $ evalPrefixExpression env JSDecrement (JSRowVal (JSAtom "y"))
--     val    <- runExceptT $ getVar env "y"
--     print (result, val)
-- :}
-- (Right 2.25,Right 2.25)
--
-- >>> runExceptT $ evalPrefixExpression [] JSDecrement (JSRowVal (JSInt 3))
-- Left Undecreasable expression: 3
evalPrefixExpression :: JSEnv
                     -> JSOperator
                     -> JSExpression
                     -> IOThrowsError JSVal
evalPrefixExpression env JSLogicNot expr =
  do val <- eval env expr
     jsLogicNot val
evalPrefixExpression env JSIncrement (JSRowVal (JSAtom var)) =
  do oldVal <- getVar env var
     newVal <- jsStrictMathOperate oldVal jsOne jsAdd
     setVar env var newVal
evalPrefixExpression _   JSIncrement expr =
  throwError $ BadSpecialForm "Unincreasable expression" expr
evalPrefixExpression env JSDecrement (JSRowVal (JSAtom var)) =
  do oldVal <- getVar env var
     newVal <- jsStrictMathOperate oldVal jsOne jsSubtract
     setVar env var newVal
evalPrefixExpression _   JSDecrement expr =
  throwError $ BadSpecialForm "Undecreasable expression" expr
evalPrefixExpression _ op _ =
  throwError . Default $ show op `mappend` " is not a prefix operator"

-- | evaluate an expression with a suffix operator
--
-- Examples:
-- >>> runExceptT $ evalSuffixExpression [] JSIncrement (JSRowVal (JSAtom "x"))
-- Left Try to get an unbound variable: x
--
-- >>> :{
--  do env    <- initEnv [("x", JSInt 5)]
--     result <- runExceptT $ evalSuffixExpression env JSIncrement (JSRowVal (JSAtom "x"))
--     val    <- runExceptT $ getVar env "x"
--     print (result, val)
-- :}
-- (Right 5,Right 6)
--
-- >>> :{
--  do env    <- initEnv [("x", JSInt 5), ("y", JSFloat 3.25)]
--     result <- runExceptT $ evalSuffixExpression env JSDecrement (JSRowVal (JSAtom "y"))
--     val    <- runExceptT $ getVar env "y"
--     print (result, val)
-- :}
-- (Right 3.25,Right 2.25)
--
-- >>> runExceptT $ evalSuffixExpression [] JSDecrement (JSRowVal (JSInt 3))
-- Left Undecreasable expression: 3
evalSuffixExpression :: JSEnv
                     -> JSOperator
                     -> JSExpression
                     -> IOThrowsError JSVal
evalSuffixExpression env JSIncrement (JSRowVal (JSAtom var)) =
  do oldVal <- getVar env var
     newVal <- jsStrictMathOperate oldVal jsOne jsAdd
     _      <- setVar env var newVal
     return oldVal
evalSuffixExpression _   JSIncrement expr =
  throwError $ BadSpecialForm "Unincreasable expression" expr
evalSuffixExpression env JSDecrement (JSRowVal (JSAtom var)) =
  do oldVal <- getVar env var
     newVal <- jsStrictMathOperate oldVal jsOne jsSubtract
     _      <- setVar env var newVal
     return oldVal
evalSuffixExpression _   JSDecrement expr =
  throwError $ BadSpecialForm "Undecreasable expression" expr
evalSuffixExpression _ op _ =
  throwError . Default $ show op `mappend` " is not a suffix operator"

-- | !!! eval js function call, function should be eval in analyzer
-- the second argument should result a closure
evalFunctionCall :: JSEnv
                 -> JSExpression
                 -> [JSExpression]
                 -> IOThrowsError JSVal
evalFunctionCall env closure args = throwError $ Default "Unfinished evaluation"

-- | fetch a field of the given js value, the field should be represented as
-- a js atom, otherwise a BadSpecialForm error will occur
--
-- Examples:
-- >>> let obj       = JSRowVal (JSObject [("x", JSInt 0), ("y", JSNull)])
-- >>> let validF    = JSRowVal (JSAtom "y")
-- >>> let invalidF1 = JSRowVal (JSAtom  "z")
-- >>> let invalidF2 = JSRowVal (JSInt 2)
-- >>> :{
--  do res1 <- runExceptT $ evalDotFieldGet [] obj validF
--     res2 <- runExceptT $ evalDotFieldGet [] obj invalidF1
--     res3 <- runExceptT $ evalDotFieldGet [] obj invalidF2
--     print (res1, res2, res3)
-- :}
-- (Right null,Right undefined,Left Unrecognized special form: [object Object].2)
--
-- >>> let notObj   = JSRowVal (JSInt 3)
-- >>> let fooField = JSRowVal (JSAtom "x")
-- >>> runExceptT $ evalDotFieldGet [] notObj fooField
-- Left Try fetching field of a invalid form: 3
evalDotFieldGet :: JSEnv
                -> JSExpression
                -> JSExpression
                -> IOThrowsError JSVal
evalDotFieldGet env objExpr (JSRowVal (JSAtom field)) =
  do objValue <- eval env objExpr
     case objValue of
       JSObject pairs ->
         maybe (return JSUndefined) return (lookup field pairs)
       _            ->
         throwError $ BadSpecialForm
                      "Try fetching field of a invalid form" objExpr
evalDotFieldGet env objExpr fieldExpr =
  throwError $ BadSpecialForm "Unrecognized special form"
    (JSDotFieldGet objExpr fieldExpr)

-- | fetch a field of the given js value, the field should be represented as
-- either a js string or a js int
-- the field of a js object can be fetched by the field name written in a
-- js string, the element of an array can be fetched by its index, if the
-- index is not out of range.
-- try to fetch a field in any other way will get a js undefined as result
--
-- Examples:
-- >>> let arr     = JSRowVal (JSArray [JSInt 0, JSInt 1])
-- >>> let inIdx   = JSRowVal (JSInt 1)
-- >>> let outIdx1 = JSRowVal (JSInt 2)
-- >>> let outIdx2 = JSRowVal (JSInt (-1))
-- >>> :{
--  do res1 <- runExceptT $ evalBracketFiledGet [] arr inIdx
--     res2 <- runExceptT $ evalBracketFiledGet [] arr outIdx1
--     res3 <- runExceptT $ evalBracketFiledGet [] arr outIdx2
--     print (res1, res2, res3)
-- :}
-- (Right 1,Right undefined,Right undefined)
--
-- >>> let obj       = JSRowVal (JSObject [("x", JSInt 0), ("y", JSNull)])
-- >>> let validF    = JSRowVal (JSString "y")
-- >>> let invalidF1 = JSRowVal (JSString "z")
-- >>> let invalidF2 = JSRowVal (JSInt 2)
-- >>> :{
--  do res1 <- runExceptT $ evalBracketFiledGet [] obj validF
--     res2 <- runExceptT $ evalBracketFiledGet [] obj invalidF1
--     res3 <- runExceptT $ evalBracketFiledGet [] obj invalidF2
--     print (res1, res2, res3)
-- :}
-- (Right null,Right undefined,Right undefined)
--
-- >>> runExceptT $ evalBracketFiledGet [] (JSRowVal (JSInt 3)) (JSRowVal (JSString "x"))
-- Right undefined
evalBracketFiledGet :: JSEnv
                    -> JSExpression
                    -> JSExpression
                    -> IOThrowsError JSVal
evalBracketFiledGet env objExpr fieldExpr =
  do objVal   <- eval env objExpr
     fieldVal <- eval env fieldExpr
     case (objVal, fieldVal) of
       (JSObject pairs, JSString field) ->
         maybe (return JSUndefined) return (lookup field pairs)
       (JSArray  elems, JSInt    index) ->
         if index >= 0 && fromInteger index < length elems
           then return (elems !! fromInteger index)
           else return JSUndefined
       _ -> return JSUndefined

-- | eval a js object literal expression to a js object value
--
-- Examples:
-- >>> runExceptT $ evalObjectLiteral [] []
-- Right [object Object]
-- >>> runExceptT $ evalObjectLiteral [] [("x", JSRowVal (JSInt 3)), ("y", JSRowVal (JSInt 5))]
-- Right [object Object]
--
-- >>> let expr1 = JSPrefixUnaryOperate JSIncrement (JSRowVal (JSAtom "x"))
-- >>> :{
--  do env <- initEnv [("x", JSInt 3)]
--     res <- runExceptT $ evalObjectLiteral env [("a", expr1), ("b", expr1), ("c", expr1)]
--     val <- runExceptT $ getVar env "x"
--     print (res, val)
-- :}
-- (Right [object Object],Right 6)
evalObjectLiteral :: JSEnv -> [(String, JSExpression)] -> IOThrowsError JSVal
evalObjectLiteral env keyExprPairs =
  do keyValuePairs <- mapM evalPair keyExprPairs
     return $ JSObject keyValuePairs
    where
      evalPair :: (String, JSExpression) -> IOThrowsError (String, JSVal)
      evalPair (key, expr) = do value <- eval env expr
                                return (key, value)

-- | eval a js array literal expression to a js array value
--
-- Examples:
-- >>> runExceptT $ evalArrayLiteral [] []
-- Right
-- >>> runExceptT $ evalArrayLiteral [] [JSRowVal (JSInt 3), JSRowVal (JSInt 5)]
-- Right 3,5
--
-- >>> let expr1 = JSPrefixUnaryOperate JSIncrement (JSRowVal (JSAtom "x"))
-- >>> :{
--  do env <- initEnv [("x", JSInt 3)]
--     res <- runExceptT $ evalArrayLiteral env [expr1, expr1, expr1]
--     val <- runExceptT $ getVar env "x"
--     print (res, val)
-- :}
-- (Right 4,5,6,Right 6)
evalArrayLiteral :: JSEnv -> [JSExpression] -> IOThrowsError JSVal
evalArrayLiteral env elems =
  do vals <- mapM (eval env) elems
     return $ JSArray vals
