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
                 deriving (Show, Eq, Ord)
type JsIdentifier = String

data JsExpr = JsIntLit Int
            | JsRef JsIdentifier
            | JsPlus JsExpr JsExpr
            | JsMinus JsExpr JsExpr
            | JsTimes JsExpr JsExpr
            | JsCall JsExpr [JsExpr]
            | JsLambda [JsIdentifier] [JsStatement]
            deriving (Show, Eq, Ord)

jsToString :: JsProgram -> String
jsToString = foldr ((++) . jsStatementToString) ""

jsStatementToString :: JsStatement -> String
jsStatementToString (JsAssign i e) = concat [ "var ", i, " = "
                                            , exprToStr e, ";"]
jsStatementToString (JsExprStatement e) = exprToStr e ++ ";"

exprToStr :: JsExpr -> String
exprToStr (JsIntLit i) = show i
exprToStr (JsRef r) = r
exprToStr (JsPlus a b) = exprToStr a ++ " + " ++ exprToStr b
exprToStr (JsMinus a b) = exprToStr a ++ " - " ++ exprToStr b
exprToStr (JsTimes a b) = exprToStr a ++ " * " ++ exprToStr b
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
