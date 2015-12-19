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

          | Greater Expr Expr
          | GreaterEq Expr Expr
          | Less Expr Expr
          | LessEq Expr Expr
          | Eq Expr Expr
          | NotEq Expr Expr

          | And Expr Expr
          | Or Expr Expr

          | Not Expr

          | If Expr Expr Expr

          | Call Expr [Expr]
          | Lambda [Identifier] [Expr]

          | Array [Expr]

          | Print Expr
          | Let [(Identifier, Expr)] [Expr]
          deriving (Show, Eq, Ord)
