module Interpret
    ( interpret
    )
  where

import Ast
import Control.Monad.Trans.State
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.IO.Class
import Control.Arrow (first)

interpret :: Program -> IO (Maybe String)
interpret p =
    let ftab = buildFunctionTable p
    in case M.lookup "main" ftab of
         Nothing -> return $ Just "function \"main\" is not defined"
         Just main -> do
           result <- evalStateT (runEvalLisp (call main [])) (ftab, ftab)
           case result of
             Left e -> return $ Just e
             _ -> return Nothing

----

type SymTab = Map String Value
type FunTab = SymTab

data Value = IntVal Integer
           | BoolVal Bool
           | ArrayVal [Value]
           | LambdaVal [Identifier] [Expr]

instance Show Value where
    show (IntVal a) = show a
    show (BoolVal a) = show a
    show (ArrayVal a) = "[" ++ unwords (map show a) ++ "]"
    show (LambdaVal params body) =
      show $ Lambda params body

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

evalExprs :: [Expr] -> StateT (SymTab, FunTab) IO (Either String Value)
evalExprs [] = return $ Left "No expressions to evaluate"
evalExprs [e] = runEvalLisp $ eval e
evalExprs (e : es) = do
    e' <- runEvalLisp $ eval e
    case e' of
      Left err' -> return $ Left err'
      Right _ -> evalExprs es

bindArgs :: (Monad a) => [(Identifier, Value)] -> StateT (SymTab, FunTab) a ()
bindArgs [] = return ()
bindArgs ((p, a) : rest) = do
    update $ first $ M.insert p a
    bindArgs rest

update :: Monad m => (a -> a) -> StateT a m ()
update f = do
    s <- get
    let s' = f s
    put s'

eval :: Expr -> EvalLisp Value
eval (IntLit a) = return $ IntVal a
eval (Ref a) = EvalLisp $ do
    (vtab, ftab) <- get
    runEvalLisp $ case M.lookup a vtab of
                    Just x -> EvalLisp $ return $ Right x
                    Nothing -> case M.lookup a ftab of
                                 Just x -> EvalLisp $ return $ Right x
                                 Nothing -> err $ a ++ " is undefined"
eval (Plus a b)      = (+) <$> eval a <*> eval b
eval (Minus a b)     = (-) <$> eval a <*> eval b
eval (Times a b)     = (*) <$> eval a <*> eval b
eval (Greater a b)   = ((BoolVal .) . (>))  <$> eval a <*> eval b
eval (GreaterEq a b) = ((BoolVal .) . (>=)) <$> eval a <*> eval b
eval (Less a b)      = ((BoolVal .) . (<))  <$> eval a <*> eval b
eval (LessEq a b)    = ((BoolVal .) . (<=)) <$> eval a <*> eval b
eval (Eq a b)        = ((BoolVal .) . (==)) <$> eval a <*> eval b
eval (NotEq a b)     = ((BoolVal .) . (/=)) <$> eval a <*> eval b
eval (And a b)       = boolValAnd <$> eval a <*> eval b
eval (Or a b)        = boolValOr <$> eval a <*> eval b
eval (Not a)         = boolValNot <$> eval a
eval (If cond thenB elseB) = do
  cond' <- eval cond
  case cond' of
    (BoolVal True) -> eval thenB
    _ -> eval elseB
eval (Call e params) = do
    e' <- eval e
    params' <- mapM eval params
    call e' params'
eval (Lambda args body) = return $ LambdaVal args body
eval (Array elements) = do
    x <- mapM eval elements
    return $ ArrayVal x
eval (Print a) = do
    a' <- eval a
    liftIO $ print a'
    return a'
eval (Let [] body) = EvalLisp $ evalExprs body
eval (Let ((i, e) : bindings) body) = do
    v <- eval e
    EvalLisp $ do
      update $ first $ M.insert i v
      runEvalLisp $ eval (Let bindings body)

call :: Value -> [Value] -> EvalLisp Value
call (LambdaVal params _) args | length params /= length args = err "Wrong number of arguments"
call (LambdaVal params body) args = EvalLisp $ do
    prev@(_, funs) <- get
    put (M.empty, funs)
    bindArgs $ zip params args
    result <- evalExprs body
    put prev
    return result
call (ArrayVal vals) [IntVal n] = case vals `safeIdx` fromIntegral n of
                                    Nothing -> err "Array index failure"
                                    Just x -> return x
call (ArrayVal _) _ = err "Wrong number of arguments to array"
call i _ = err $ show i ++ " is not a function"

safeIdx :: [a] -> Int -> Maybe a
safeIdx [] _ = Nothing
safeIdx (x : _) 0 = Just x
safeIdx (_ : rest) n = safeIdx rest (n - 1)

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

newtype EvalLisp a = EvalLisp { runEvalLisp :: StateT (SymTab, FunTab) IO (Either String a) }

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

err :: String -> EvalLisp a
err = EvalLisp . return . Left

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
