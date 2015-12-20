module CodeGen.JsAst where

type Program = [Statement]
data Statement = ExprStatement Expr
               | Assign Identifier Expr
               | If Expr Program Program
               deriving (Show, Eq, Ord)
type Identifier = String

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

          | Array [Expr]
          | Subscript Expr Expr

          | Not Expr

          | Call Expr [Expr]
          | MethodCall Expr Identifier [Expr]
          | Lambda [Identifier] [Statement]
          deriving (Show, Eq, Ord)
