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
          deriving (Eq, Ord)

-- | Pretty printing

instance Show Expr where
    show (IntLit i) = show i
    show (Ref a) = a
    show (Plus a b)      = paren2 "+" a b
    show (Minus a b)     = paren2 "-" a b
    show (Times a b)     = paren2 "*" a b
    show (Greater a b)   = paren2 "<" a b
    show (GreaterEq a b) = paren2 "<= " a b
    show (Less a b)      = paren2 ">" a b
    show (LessEq a b)    = paren2 ">= " a b
    show (Eq a b)        = paren2 "= " a b
    show (NotEq a b)     = paren2 "/= " a b
    show (And a b)       = paren2 "&&" a b
    show (Or a b)        = paren2 "||" a b
    show (Not a)         = paren1 "!" a
    show (If a b c) = "(if " ++ show a ++ " " ++ show b  ++ " " ++ show c ++ ")"
    show (Call a bs) = "(" ++ show a ++ " " ++ unwords (show <$> bs) ++ ")"
    show (Lambda as bs) = "(lambda (" ++ unwords (map show as) ++ ") " ++ unwords (show <$> bs) ++ ")"
    show (Print a) = "(print " ++ show a ++ ")"
    show (Array as) = "[" ++ unwords (show <$> as) ++ "]"
    show (Let as bs) = "(let (" ++ unwords (map (uncurry paren1) as) ++ ") " ++ unwords (show <$> bs) ++ ")"

paren2 :: (Show a, Show b) => String -> a -> b -> String
paren2 f a b = "(" ++ f ++ " " ++ show a ++ " " ++ show b ++ ")"

paren1 :: (Show a) => String -> a -> String
paren1 f a = "(" ++ f ++ " " ++ show a ++ ")"
