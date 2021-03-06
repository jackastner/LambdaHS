module Parser where

import Types

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans
import Control.Monad.Except

lookahead :: ParserState Token
lookahead = head <$> get

throwParseError :: (Show b, Show c) => b -> c -> ParserState a
throwParseError expected found = lift . throwError $ "PARSE ERROR while matching " ++ (show expected) ++ ". Unexpected token " ++ (show found)
 
matchToken :: Token -> ParserState Token
matchToken match = do 
  tok <- lookahead
  case match of 
    (TOK_ATOM _) -> case tok of
      (TOK_ATOM _) -> uncheckedMatchToken >> return tok
      _ -> throwParseError match tok
    _ | tok == match -> uncheckedMatchToken >> return tok
    _ -> throwParseError match tok
  where uncheckedMatchToken = (tail <$> get) >>= put

parseExpression :: ParserState Expression
parseExpression = do
  tok <- lookahead
  case tok of
    TOK_LPAREN -> do
      matchToken TOK_LPAREN
      tok <- lookahead
      case tok of
        TOK_LAMBDA -> do
         matchToken TOK_LAMBDA
         (TOK_ATOM id) <- matchToken (TOK_ATOM "")
         matchToken TOK_DOT
         exp <- parseExpression
         matchToken TOK_RPAREN
         return $ Lambda id exp
        _ -> do
          exp0 <- parseExpression
          exp1 <- parseExpression
          matchToken TOK_RPAREN
          return $ Apply exp0 exp1
    TOK_ATOM id -> matchToken (TOK_ATOM id) >> (return $ Var id)
    _ -> throwParseError [TOK_LPAREN, TOK_ATOM ""] tok
