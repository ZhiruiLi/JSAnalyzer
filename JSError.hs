module JSError
( JSError (..)
, ThrowsError
, IOThrowsError
, liftThrows
, throwError
, liftIO
, runExceptT
) where

import           Control.Monad.Except
import           JSEntity              (JSExpression, JSStatement, JSVal)
import           Text.Megaparsec.Error (ParseError)

data JSError = NumArgs          Integer    [JSVal]
             | TypeMismatch     String     JSVal
             | ParserError      ParseError
             | BadSpecialForm   String     JSExpression
             | NotFunction      JSVal
             | UnboundVar       String     String
             | NameConflict     String
             | InvalidStatement (Maybe JSStatement)
             | Default          String

unwordsList :: Show s => [s] -> String
unwordsList  = unwords . fmap show

instance Show JSError where
  show err = case err of
    NumArgs expected actualVals ->
      "Expected "
        `mappend` show expected
        `mappend` " args: found values "
        `mappend` unwordsList [actualVals]
    TypeMismatch expected actualVal ->
      "Invalid type: expected "
        `mappend` expected
        `mappend` ", found "
        `mappend` show actualVal
    ParserError e ->
      "Parse error at " `mappend` show e
    BadSpecialForm msg expr ->
      msg `mappend` ": " `mappend` show expr
    NotFunction func ->
      "Try to call a non-function value: " `mappend` show func
    UnboundVar msg varName ->
      msg `mappend` ": " `mappend` varName
    NameConflict varName ->
      "Name conflict: "
        `mappend` varName
        `mappend` " has already been defined"
    InvalidStatement maybeStmt ->
      "Invalid statement: " `mappend` case maybeStmt of
        Nothing   -> "unknown"
        Just stmt -> show stmt
    Default s -> s

type ThrowsError = Either JSError

type IOThrowsError = ExceptT JSError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val
