module Main where

import           JSAnalyzer  (analyzeProgram)
import           JSError
import           JSEvaluator (initEnv)
import           JSParser    (jsProgramParser, parseString)

printList :: Show a => [a] -> IO()
printList []     = return ()
printList (x:xs) = do print x
                      printList xs

main :: IO()
main = do
  print "Please input JS file path:"
  path     <- getLine
  content  <- readFile path
  case parseString jsProgramParser content of
    Left  err   -> print err
    Right prog  ->
      do env <- initEnv []
         res <- runExceptT $ analyzeProgram env prog
         case res of
           Left  err   -> print err
           Right warns -> printList warns
