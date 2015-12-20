module CodeGen.JsToStr
    ( jsToString
    )
  where

import qualified CodeGen.JsAst as JS
import Data.List

jsToString :: JS.Program -> String
jsToString = foldr ((++) . jsStatementToStr) ""

jsStatementToStr :: JS.Statement -> String
jsStatementToStr (JS.Assign i e) = concat [ "var ", i, " = "
                                          , exprToStr e, ";"]
jsStatementToStr (JS.ExprStatement e) = exprToStr e ++ ";"
jsStatementToStr (JS.If cond thenB elseB) =
    concat [ "if (", exprToStr cond, ")", "{"
           , jsFunBodyToStr thenB
           , "} else {"
           , jsFunBodyToStr elseB
           , "}"
           ]

exprToStrSym :: String -> JS.Expr -> JS.Expr -> String
exprToStrSym s a b = exprToStr a ++ s ++ exprToStr b

exprToStr :: JS.Expr -> String
exprToStr (JS.IntLit i) = show i
exprToStr (JS.Ref r) = r

exprToStr (JS.Plus a b) = exprToStrSym " + " a b
exprToStr (JS.Minus a b) = exprToStrSym " - " a b
exprToStr (JS.Times a b) = exprToStrSym " * " a b

exprToStr (JS.Greater a b) = exprToStrSym " > " a b
exprToStr (JS.GreaterEq a b) = exprToStrSym " >= " a b
exprToStr (JS.Less a b) = exprToStrSym " < " a b
exprToStr (JS.LessEq a b) = exprToStrSym " <= " a b
exprToStr (JS.Eq a b) = exprToStrSym " === " a b
exprToStr (JS.NotEq a b) = exprToStrSym " !== " a b

exprToStr (JS.And a b) = exprToStrSym " && " a b
exprToStr (JS.Or a b) = exprToStrSym " || " a b

exprToStr (JS.Not a) = "!(" ++ exprToStr a ++ ")"

exprToStr (JS.Array es) = "[" ++ elements ++ "]"
  where elements = intercalate ", " $ map exprToStr es

exprToStr (JS.Subscript array idx) = exprToStr array ++ "[" ++ exprToStr idx ++ "]"

exprToStr (JS.Call f args) = exprToStr f ++ "(" ++ args' ++ ")"
  where args' = intercalate ", " $ map exprToStr args
exprToStr (JS.MethodCall receiver selector args) =
    let args' = intercalate ", " $ map exprToStr args
    in concat [ exprToStr receiver, ".", selector, "(", args', ")"]
exprToStr (JS.Lambda args body) = "(function(" ++ args' ++ ") {" ++ body' ++ "})"
  where args' = intercalate ", " args
        body' = jsFunBodyToStr body

jsFunBodyToStr :: [JS.Statement] -> String
jsFunBodyToStr [] = ""
jsFunBodyToStr [JS.ExprStatement e] = "return " ++ exprToStr e
jsFunBodyToStr (e : rest) = jsStatementToStr e ++ jsFunBodyToStr rest
