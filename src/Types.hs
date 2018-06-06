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

prettyPrint :: Expression -> String
prettyPrint (Lambda id exp) = "(\\ " ++ id ++ " . " ++ (prettyPrint exp) ++ " )"
prettyPrint (Apply exp0 exp1) = "( " ++ (prettyPrint exp0) ++ " " ++ (prettyPrint exp1) ++ " )"
prettyPrint (Var id) = id
