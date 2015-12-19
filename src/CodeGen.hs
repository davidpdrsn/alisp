module CodeGen
    ( genCode
    )
  where

import Ast
import Data.List

genCode :: Program -> String
genCode p = functions ++ callMain
    where functions = jsToString $ genCodeJS p
          callMain = jsStatementToString $ JsExprStatement $ JsCall (JsRef "main") []

-- | Compile JavaScript

type JsProgram = [JsStatement]
data JsStatement = JsExprStatement JsExpr
                 | JsAssign JsIdentifier JsExpr
                 | JsIf JsExpr JsProgram JsProgram
                 deriving (Show, Eq, Ord)
type JsIdentifier = String

data JsExpr = JsIntLit Int
            | JsRef JsIdentifier

            | JsPlus JsExpr JsExpr
            | JsMinus JsExpr JsExpr
            | JsTimes JsExpr JsExpr

            | JsGreater JsExpr JsExpr
            | JsGreaterEq JsExpr JsExpr
            | JsLess JsExpr JsExpr
            | JsLessEq JsExpr JsExpr
            | JsEq JsExpr JsExpr
            | JsNotEq JsExpr JsExpr

            | JsAnd JsExpr JsExpr
            | JsOr JsExpr JsExpr

            | JsNot JsExpr

            | JsCall JsExpr [JsExpr]
            | JsLambda [JsIdentifier] [JsStatement]
            deriving (Show, Eq, Ord)

jsToString :: JsProgram -> String
jsToString = foldr ((++) . jsStatementToString) ""

jsStatementToString :: JsStatement -> String
jsStatementToString (JsAssign i e) = concat [ "var ", i, " = "
                                            , exprToStr e, ";"]
jsStatementToString (JsExprStatement e) = exprToStr e ++ ";"
jsStatementToString (JsIf cond thenB elseB) =
    concat [ "if (", exprToStr cond, ")", "{"
           , jsFunBodyToStr thenB
           , "} else {"
           , jsFunBodyToStr elseB
           , "}"
           ]

exprToStrSym :: String -> JsExpr -> JsExpr -> String
exprToStrSym s a b = exprToStr a ++ s ++ exprToStr b

exprToStr :: JsExpr -> String
exprToStr (JsIntLit i) = show i
exprToStr (JsRef r) = r

exprToStr (JsPlus a b) = exprToStrSym "-" a b
exprToStr (JsMinus a b) = exprToStrSym " - " a b
exprToStr (JsTimes a b) = exprToStrSym " * " a b

exprToStr (JsGreater a b) = exprToStrSym " > " a b
exprToStr (JsGreaterEq a b) = exprToStrSym " >= " a b
exprToStr (JsLess a b) = exprToStrSym " < " a b
exprToStr (JsLessEq a b) = exprToStrSym " <= " a b
exprToStr (JsEq a b) = exprToStrSym " === " a b
exprToStr (JsNotEq a b) = exprToStrSym " !== " a b

exprToStr (JsAnd a b) = exprToStrSym " && " a b
exprToStr (JsOr a b) = exprToStrSym " || " a b

exprToStr (JsNot a) = "!(" ++ exprToStr a ++ ")"

exprToStr (JsCall f args) = exprToStr f ++ "(" ++ args' ++ ")"
  where args' = intercalate ", " $ map exprToStr args
exprToStr (JsLambda args body) = "(function(" ++ args' ++ ") {" ++ body' ++ "})"
  where args' = intercalate ", " args
        body' = jsFunBodyToStr body

jsFunBodyToStr :: [JsStatement] -> String
jsFunBodyToStr [] = ""
jsFunBodyToStr [JsExprStatement e] = "return " ++ exprToStr e
jsFunBodyToStr (e : rest) = jsStatementToString e ++ jsFunBodyToStr rest

genCodeJS :: Program -> JsProgram
genCodeJS = map compileFun

-- | Compile AST

compileFun :: Function -> JsStatement
compileFun f = JsAssign name (JsLambda args body)
  where name = funName f
        args = funArgs f
        body = map (JsExprStatement . compileExpr) $ funBody f

compileExpr :: Expr -> JsExpr
compileExpr (IntLit i) = JsIntLit i
compileExpr (Ref r) = JsRef r

compileExpr (Plus a b) = JsPlus (compileExpr a) (compileExpr b)
compileExpr (Minus a b) = JsMinus (compileExpr a) (compileExpr b)
compileExpr (Times a b) = JsTimes (compileExpr a) (compileExpr b)

compileExpr (Greater a b) = JsGreater (compileExpr a) (compileExpr b)
compileExpr (GreaterEq a b) = JsGreaterEq (compileExpr a) (compileExpr b)
compileExpr (Less a b) = JsLess (compileExpr a) (compileExpr b)
compileExpr (LessEq a b) = JsLessEq (compileExpr a) (compileExpr b)
compileExpr (Eq a b) = JsEq (compileExpr a) (compileExpr b)
compileExpr (NotEq a b) = JsNotEq (compileExpr a) (compileExpr b)

compileExpr (And a b) = JsAnd (compileExpr a) (compileExpr b)
compileExpr (Or a b) = JsOr (compileExpr a) (compileExpr b)

compileExpr (Not a) = JsNot (compileExpr a)

compileExpr (If cond thenB elseB) = JsCall lambda []
    where lambda = JsLambda [] [body]
          body = JsIf (compileExpr cond) [thenB'] [elseB']
          thenB' = JsExprStatement $ compileExpr thenB
          elseB' = JsExprStatement $ compileExpr elseB

compileExpr (Lambda args body) = JsLambda args $ map (JsExprStatement . compileExpr) body
compileExpr (Call f args) = JsCall (JsRef f) (map compileExpr args)

compileExpr (Let bindings exprs) =
    let bindings' = map (uncurry compileBinding) bindings
        exprs' = map (JsExprStatement . compileExpr) exprs
        lambda = JsLambda [] (bindings' ++ exprs')
    in JsCall lambda []
compileExpr (Print a) = JsCall lambda []
  where lambda = JsLambda [] [consoleLog, returnStatment]
        a' = compileExpr a
        consoleLog = JsExprStatement $ JsCall (JsRef "console.log") [a']
        returnStatment = JsExprStatement a'

compileBinding :: Identifier -> Expr -> JsStatement
compileBinding i e = JsAssign i $ compileExpr e
