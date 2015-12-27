module Interpret
    ( interpret
    )
  where

import Ast
import Control.Monad.Trans.State
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.IO.Class

interpret :: Program -> IO (Maybe String)
interpret p =
    let ftab = buildFunctionTable p
    in case M.lookup "main" ftab of
         Nothing -> return $ Just "function \"main\" is not defined"
         Just main -> do
           result <- evalStateT (runEvalLisp $ call main []) (buildFunctionTable p)
           case result of
             Left e -> return $ Just e
             _ -> return Nothing

----

type SymTab = Map String Value

data Value = IntVal Int
           | BoolVal Bool
           | ArrayVal [Value]
           | LambdaVal [Identifier] [Expr]

instance Show Value where
    show (IntVal a) = show a
    show (BoolVal a) = show a
    show (ArrayVal a) = "[" ++ intercalate ", " (map show a) ++ "]"

instance Num Value where
    (IntVal a) + (IntVal b) = IntVal $ a + b
    _ + _ = error "Type error: Should have been caught by the type checker"
    (IntVal a) - (IntVal b) = IntVal $ a - b
    _ - _ = error "Type error: Should have been caught by the type checker"
    (IntVal a) * (IntVal b) = IntVal $ a * b
    _ * _ = error "Type error: Should have been caught by the type checker"
    abs (IntVal a) = IntVal $ abs a
    abs _ = error "Type error: Should have been caught by the type checker"
    signum (IntVal a) = IntVal $ signum a
    signum _ = error "Type error: Should have been caught by the type checker"
    fromInteger = IntVal . fromInteger

instance Ord Value where
    compare (IntVal a) (IntVal b) = compare a b
    compare (BoolVal a) (BoolVal b) = compare a b
    compare _ _ = error "Type error: Should have been caught by the type checker"

instance Eq Value where
    (==) (IntVal a) (IntVal b) = a == b
    (==) (BoolVal a) (BoolVal b) = a == b
    (==) _ _ = error "Type error: Should have been caught by the type checker"

evalExprs :: [Expr] -> StateT SymTab IO (Either String Value)
evalExprs = foldM (\_ expr -> runEvalLisp $ eval expr) (Right $ IntVal 0) -- TODO: acc not used?!

bindArgs :: (Monad a) => [(Identifier, Value)] -> StateT SymTab a ()
bindArgs [] = return ()
bindArgs ((p, a) : rest) = do
    update $ M.insert p a
    bindArgs rest

update f = do
    s <- get
    let s' = f s
    put s'

wrongArgMessage :: Int -> Int -> String
wrongArgMessage required given =
    "Wrong number of arguments, " ++ show required ++ " given, " ++ show given ++ " given"

bindParams :: [Identifier] -> [Value] -> Either (Int, Int) [(Identifier, Value)]
bindParams params args = if length params == length args
                           then Right $ zip params args
                           else Left (length params, length args)

eval :: Expr -> EvalLisp Value
eval (IntLit a) = return $ IntVal a
eval (Ref a) = EvalLisp $ do
    tab <- get
    case M.lookup a tab of
      Just x -> return $ Right x
      Nothing -> return $ Left $ a ++ " is undefined"
eval (Plus a b) = (+) <$> eval a <*> eval b
eval (Minus a b) = (-) <$> eval a <*> eval b
eval (Times a b) = (*) <$> eval a <*> eval b
eval (Greater a b) = ((BoolVal .) . (>)) <$> eval a <*> eval b
eval (GreaterEq a b) = ((BoolVal .) . (>=)) <$> eval a <*> eval b
eval (Less a b) = ((BoolVal .) . (<)) <$> eval a <*> eval b
eval (LessEq a b) = ((BoolVal .) . (<=)) <$> eval a <*> eval b
eval (Eq a b) = ((BoolVal .) . (==)) <$> eval a <*> eval b
eval (NotEq a b) = ((BoolVal .) . (/=)) <$> eval a <*> eval b
eval (And a b) = boolValAnd <$> eval a <*> eval b
eval (Or a b) = boolValOr <$> eval a <*> eval b
eval (Not a) = boolValNot <$> eval a
eval (If cond thenB elseB) = do
  cond' <- eval cond
  case cond' of
    (BoolVal True) -> eval thenB
    _ -> eval elseB
eval (Call e params) = do
    e' <- eval e
    params' <- mapM eval params
    call e' params'
-- eval (Call e@(Lambda _ _) params) = undefined
-- eval (Call e@(Array _) params) = undefined
eval (Lambda args body) = return $ LambdaVal args body
eval (Array elements) = do
    x <- mapM eval elements
    return $ ArrayVal x
eval (Print a) = do
    a' <- eval a
    liftIO $ print a'
    return a'
eval (Let bindings body) = do
    values <- mapM (\(i, e) -> do { e' <- eval e; return (i, e') }) bindings
    EvalLisp $ do
      s <- get
      let s' = foldr (uncurry M.insert) s values
      put s'
      evalExprs body

call :: Value -> [Value] -> EvalLisp Value
call f params = undefined

boolValNot :: Value -> Value
boolValNot (BoolVal a) = BoolVal $ not a
boolValNot _ = error "Type error: Should have been caught by the type checker"

boolValAnd :: Value -> Value -> Value
boolValAnd (BoolVal a) (BoolVal b) = BoolVal $ a && b
boolValAnd _ _ = error "Type error: Should have been caught by the type checker"

boolValOr :: Value -> Value -> Value
boolValOr (BoolVal a) (BoolVal b) = BoolVal $ a || b
boolValOr _ _ = error "Type error: Should have been caught by the type checker"

buildFunctionTable :: [Function] -> SymTab
buildFunctionTable = M.fromList . map (\f -> (funName f, LambdaVal (funArgs f) (funBody f)))

-- | EvalLisp helper type

newtype EvalLisp a = EvalLisp { runEvalLisp :: StateT SymTab IO (Either String a) }

instance Functor EvalLisp where
    fmap f (EvalLisp x) = EvalLisp $ do
      x' <- x
      return $ do
        x'' <- x'
        return $ f x''

instance Applicative EvalLisp where
    pure = EvalLisp . pure . Right
    (EvalLisp f) <*> (EvalLisp x) = EvalLisp $ do
      f' <- f
      x' <- x
      return $ do
        f'' <- f'
        x'' <- x'
        return $ f'' x''

instance Monad EvalLisp where
    return = pure
    (EvalLisp x) >>= f = EvalLisp $ do
      x' <- x
      case x' of
        Left e -> return $ Left e
        Right x'' -> runEvalLisp $ f x''

instance MonadIO EvalLisp where
    liftIO x = EvalLisp $ do
      x' <- liftIO x
      return $ Right x'
