module Eval where
import LuaTypes
import Data.Map hiding (lookup)
import Prelude hiding (lookup)

import Control.Monad.Except
import Control.Monad.State

runEval :: Env -> Eval a -> IO (Either LuaError a)
runEval env eval = runExceptT (evalStateT eval env)

runEvalDefault :: Eval a -> IO (Either LuaError a)
runEvalDefault computation = do
    env <- defaultEnv
    runEval env computation

evalBlock :: Block -> Eval [Value]
evalBlock b = do
    env <- get
    let env' = Env {bindings = empty, parent = Just env}
    put env'
    results <- mapM evalStmt (getStatements b)
    env'' <- get
    put $ Env {bindings = (case parent env'' of
                        Nothing -> empty
                        Just e -> bindings e), parent = parent env''}
    case results of
        [] -> pure []
        _ -> pure $ last results

evalStmt :: Statement -> Eval [Value]
evalStmt (Return e) = undefined
evalStmt (Local ids Nothing) = do
    env <- get
    env' <- bindAll env (zip ids (repeat Nil))
    put env'
    pure []

evalStmt (Local ids (Just vals)) = do
    evals <- mapM evalExpr vals
    let evals' = Prelude.map head (init evals) ++ last evals
    env <- get
    env' <- bindAll env (zip ids evals')
    put env'
    pure []

evalStmt (Assignment lhs es) = do
    evals <- mapM evalExpr es
    let evals' = Prelude.map head (init evals) ++ last evals
    mapM_ (\(identifier,rhs) -> do
        env <- get
        assign identifier rhs env
        ) (zip lhs evals')
    pure []

evalStmt Break = undefined
evalStmt (RepeatUntil e b) = undefined
evalStmt (If e b el m) = undefined

evalStmt (Call name es) = do
    env <- get
    var <- lookup name env
    evals <- mapM evalExpr es
    let evals' = Prelude.map head (init evals) ++ last evals
    case var of
        Builtin (BuiltinCall c) -> c evals'
        Function env' args body -> do
            undefined
        _ -> throwError $ NotCallable var

evalStmt (Do b) = evalBlock b

evalStmt (While e b) = do
    undefined

evalStmt Dummy = pure []

isTrue :: Value -> Bool
isTrue Nil = False
isTrue (Boolean False) = False
isTrue _ = True

evalExpr :: Expr -> Eval [Value]
evalExpr (EValue a) = pure [a]

evalExpr (EVar var) = do
    env <- get
    val <- lookup var env
    pure [val]

evalExpr (ECall {}) = undefined -- lookup function and call it
evalExpr (EFuncDef {}) = undefined -- add a function to env

evalExpr (EPar e) = evalExpr e
evalExpr (EBinOp And e1 e2) = do
    e1t' <- evalExpr e1
    let e1' = head e1t'
    if isTrue e1' then evalExpr e2 else pure [e1']
evalExpr (EBinOp Or e1 e2) = do
    e1t' <- evalExpr e1
    let e1' = head e1t'
    if isTrue e1' then pure [e1'] else evalExpr e2
evalExpr (EBinOp op e1 e2) = do
    e1t' <- evalExpr e1
    let e1' = head e1t'
    e2t' <- evalExpr e2
    let e2' = head e2t'
    pure $ (:[]) $ case op of
        Add -> add e1' e2'
        Sub -> sub e1' e2'
        Mul -> mul e1' e2'
        Div -> div' e1' e2'
        BitwiseAnd -> band e1' e2'
        BitwiseOr -> bor e1' e2'
        Xor -> xor' e1' e2'
        Eq -> Boolean $ e1' == e2'
        Lt -> Boolean $ e1' `lt` e2'
        Gt -> Boolean $ e2' `lt` e1'
        Ge -> Boolean $ e2' `le` e1'
        Le -> Boolean $ e1' `le` e2'
        Ne -> Boolean $ e1' /= e2'
        Concat -> String $ show e1' ++ show e2'
evalExpr (EUnOp op a) = do
    at' <- evalExpr a
    let a' = head at'
    pure $ case op of
        Minus -> [minus' a']
        Not -> [bitwiseNot' a']
        Len ->
            case a' of
                (String s) -> [Number $ fromIntegral (length s)]
                _ -> error $ "unsupported operand for length: " ++ show a'

unOp :: (Numeric -> Numeric) -> String -> Value -> Value
unOp op _ (Number a) = Number $ op a
unOp _ opStr a = error $ "unsupported operand for " ++ opStr ++ ": " ++ show a

unsupportedBinOp opStr a b =  error $ "unsupported operands for " ++ opStr ++ ": " ++ show a ++ " " ++ show b
minus' = unOp negate "unary minus"
bitwiseNot' = unOp bitwiseNot "bitwise negation"

lt (String a) (String b) = a < b
lt (Number a) (Number b) = a < b
lt a b = unsupportedBinOp "less than" a b

le (String a) (String b) = a <= b
le (Number a) (Number b) = a <= b
le a b = unsupportedBinOp "less than or equal" a b

binOp :: (Numeric -> Numeric -> Numeric) -> String -> Value -> Value -> Value
binOp op _ (Number a) (Number b) = Number (a `op` b)
binOp _ opStr a b = unsupportedBinOp opStr a b
add = binOp (+) "addition"
sub = binOp (-) "subtraction"
mul = binOp (*) "multiplication"
div' = binOp (/) "division"
band = binOp bitwiseAnd "bitwise and"
bor = binOp bitwiseOr "bitwise or"
xor' = binOp bitwiseXor "xor"
