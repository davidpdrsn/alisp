module Ast where

type Program = [Function]
type Identifier = String
data Function = Function
              { funName :: Identifier
              , funArgs :: [Identifier]
              , funBody :: [Expr]
              }
              deriving (Show, Eq, Ord)

data Expr = IntLit Int
          | Ref Identifier
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Call Identifier [Expr]
          | Print Expr
          | Let [(Identifier, Expr)] [Expr]
          deriving (Show, Eq, Ord)
