module Types where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except

type ParserState = StateT [Token] (Except String)

data Token = 
    TOK_WHITESPACE 
  | TOK_DOT
  | TOK_LPAREN
  | TOK_RPAREN
  | TOK_ATOM String
  | TOK_LAMBDA
  | TOK_EOF deriving (Eq, Ord, Show) 

data Expression = 
   Lambda String Expression
 | Apply Expression Expression
 | Var String deriving (Show)
