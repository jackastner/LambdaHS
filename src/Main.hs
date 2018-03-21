module Main where

import Parser
import Lexer
import Interpreter
import Types

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans
import Control.Monad.Except

runProgram :: String -> Either String Expression
runProgram str =  runExcept $ do
  toks <- tokenizeString lambdaTokenRegex str
  (exp,rest) <- runStateT parseExpression toks
  return $ interpretExpression exp

main :: IO ()
main = do
  result <- runProgram <$> getContents
  case result of
    (Right exp) -> do
      putStrLn "program executed succesfully"
      print exp
    (Left err) -> do
      putStrLn "program failed"
      putStrLn err
