module CodeGen.LispToJs
    ( genCode
    )
  where

import Ast
import qualified CodeGen.JsAst as JS

genCode :: Program -> JS.Program
genCode p = map compileFun p ++ [callMain]
    where callMain = JS.ExprStatement $ JS.Call (JS.Ref "main") []

compileFun :: Function -> JS.Statement
compileFun f = JS.Assign name (JS.Lambda args body)
  where name = funName f
        args = funArgs f
        body = map (JS.ExprStatement . compileExpr) $ funBody f

compileExpr :: Expr -> JS.Expr
compileExpr (IntLit i) = JS.IntLit i
compileExpr (Ref r) = JS.Ref r

compileExpr (Plus a b) = JS.Plus (compileExpr a) (compileExpr b)
compileExpr (Minus a b) = JS.Minus (compileExpr a) (compileExpr b)
compileExpr (Times a b) = JS.Times (compileExpr a) (compileExpr b)

compileExpr (Greater a b) = JS.Greater (compileExpr a) (compileExpr b)
compileExpr (GreaterEq a b) = JS.GreaterEq (compileExpr a) (compileExpr b)
compileExpr (Less a b) = JS.Less (compileExpr a) (compileExpr b)
compileExpr (LessEq a b) = JS.LessEq (compileExpr a) (compileExpr b)
compileExpr (Eq a b) = JS.Eq (compileExpr a) (compileExpr b)
compileExpr (NotEq a b) = JS.NotEq (compileExpr a) (compileExpr b)

compileExpr (And a b) = JS.And (compileExpr a) (compileExpr b)
compileExpr (Or a b) = JS.Or (compileExpr a) (compileExpr b)

compileExpr (Not a) = JS.Not (compileExpr a)

compileExpr (If cond thenB elseB) = JS.Call lambda []
    where lambda = JS.Lambda [] [body]
          body = JS.If (compileExpr cond) [thenB'] [elseB']
          thenB' = JS.ExprStatement $ compileExpr thenB
          elseB' = JS.ExprStatement $ compileExpr elseB

compileExpr (Lambda args body) = JS.Lambda args $ map (JS.ExprStatement . compileExpr) body
compileExpr (Call (Array exprs) [i]) = JS.Subscript (JS.Array $ map compileExpr exprs) (compileExpr i)
compileExpr (Call (Ref "map") [f, array]) =
    JS.MethodCall (compileExpr array) "map" [compileExpr f]
compileExpr (Call (Ref "filter") [f, array]) =
    JS.MethodCall (compileExpr array) "filter" [compileExpr f]
compileExpr (Call (Ref "fold") [f, start, array]) =
    JS.MethodCall (compileExpr array) "reduce" [compileExpr f, compileExpr start]
compileExpr (Call e args) = JS.Call (compileExpr e) (map compileExpr args)

compileExpr (Array es) = JS.Array $ map compileExpr es

compileExpr (Let bindings exprs) =
    let bindings' = map (uncurry compileBinding) bindings
        exprs' = map (JS.ExprStatement . compileExpr) exprs
        lambda = JS.Lambda [] (bindings' ++ exprs')
    in JS.Call lambda []
compileExpr (Print a) = JS.Call lambda []
  where lambda = JS.Lambda [] [consoleLog, returnStatment]
        a' = compileExpr a
        consoleLog = JS.ExprStatement $ JS.Call (JS.Ref "console.log") [a']
        returnStatment = JS.ExprStatement a'

compileBinding :: Identifier -> Expr -> JS.Statement
compileBinding i e = JS.Assign i $ compileExpr e
