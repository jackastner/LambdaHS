module Lexer where

import Types

import Text.Regex
import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad.Except

lambdaTokenRegex :: [(Token, Regex)]
lambdaTokenRegex = ((mkRegex.("^"++))<$>) <$> [
  (TOK_DOT, "\\."),
  (TOK_LPAREN, "\\("),
  (TOK_RPAREN, "\\)"),
  (TOK_ATOM "", "[a-zA-Z_]+"),
  (TOK_LAMBDA, "\\\\"),
  (TOK_WHITESPACE, "\\s+")]

matchLongest :: [(Token, Regex)] -> String -> Except String (Token, String, String)
matchLongest tokens str = headExcept ("Failed to tokenize at: " ++ (take 10 str)) . 
                          sortBy (flip $ comparing (\(tok,match,rest) -> (length match, tok))) . 
                          fmap (\(tok,(_,match,rest,_)) -> (tok, match, rest)) . 
                          catMaybes . 
                          map (\(tok,re) -> (\a-> (tok,a)) <$> matchRegexAll re str) 
                          $ tokens

headExcept  :: b -> [a] -> Except b a
headExcept b [] = throwError b
headExcept _ (h:_) = return h

tokenizeString :: [(Token, Regex)] -> String -> Except String [Token]
tokenizeString _ [] = return [TOK_EOF]
tokenizeString tokens str = do
  (tok,match,rest) <- matchLongest tokens str
  toks <- tokenizeString tokens rest
  return $ case tok of
    TOK_ATOM _ -> (TOK_ATOM match):toks
    TOK_WHITESPACE -> toks
    _ -> tok:toks
