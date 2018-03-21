module Interpreter where

import Types

interpretExpression :: Expression -> Expression
interpretExpression (Lambda str exp) = Lambda str (interpretExpression exp)
interpretExpression (Var id) = Var id
interpretExpression (Apply e0 e1) = case (interpretExpression e0) of
  Lambda binding e0' -> interpretExpression (substitute binding e0' e1)
  e -> (Apply e (interpretExpression e1))

substitute :: String -> Expression -> Expression -> Expression
substitute var e0@(Lambda binding e1) e2
  | var == binding = e0
  | otherwise = Lambda binding (substitute var e1 e2)
substitute var (Apply e0 e1) e2 = (Apply (substitute var e0 e2) (substitute var e1 e2))
substitute var e0@(Var id) e1
  | var == id = e1
  | otherwise = e0
