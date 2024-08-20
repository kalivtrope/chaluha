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

evalBlock :: Block -> ([Value] -> [Value]) -> Eval [Value]
evalBlock b ret = do
    env <- get
    let env' = Env {bindings = empty, parent = Just env}
    put env'
    results <- mapM (`evalStmt` ret) (getStatements b)
    env'' <- get
    put $ Env {bindings = (case parent env'' of
                        Nothing -> empty
                        Just e -> bindings e), parent = parent env''}
    case results of
        [] -> pure $ ret []
        _ -> pure $ ret $ last results

luaFlattenList :: [[Value]] -> [Value]
luaFlattenList vals = let vals' = Prelude.filter (/= []) vals in case vals' of
                    [] -> []
                    _ -> Prelude.map head (init vals') ++ last vals'

evalStmt :: Statement -> ([Value] -> [Value]) -> Eval [Value]
evalStmt (Return e) _ = do
    evals <- mapM (`evalExpr` id) e
    let evals' = luaFlattenList evals
    pure evals'

evalStmt (Local ids Nothing) ret = do
    env <- get
    env' <- bindAll env (zip ids (repeat Nil))
    put env'
    pure $ ret []

evalStmt (Local ids (Just vals)) ret = do
    evals <- mapM (`evalExpr` ret) vals
    let evals' = Prelude.map head (init evals) ++ last evals
    env <- get
    env' <- bindAll env (zip ids evals')
    put env'
    pure $ ret []

evalStmt (Assignment lhs es) ret = do
    evals <- mapM (`evalExpr` ret) es
    let evals' = Prelude.map head (init evals) ++ last evals
    mapM_ (\(identifier,rhs) -> do
        env <- get
        assign identifier rhs env
        ) (zip lhs evals')
    pure $ ret []

evalStmt Break _ = pure []
evalStmt (RepeatUntil e b) ret = undefined
evalStmt (If e b el m) ret = do
    val <- evalExpr e ret
    out <-
            if isTrue (last val) then
                evalBlock b ret
            else 
                do 
                    conds <- mapM (\(cond,_) -> (do evalExpr cond ret)) el
                    let condsWithBlocks = Prelude.filter (\(cond,_) -> isTrue (last cond)) $ zip conds (Prelude.map snd el)
                    case condsWithBlocks of
                        [] -> 
                            case m of 
                                Nothing -> pure $ ret []
                                Just m' -> evalBlock m' ret
                        (_,block):_ -> evalBlock block ret
    pure $ ret out

evalStmt (Call name es) ret = do
    env <- get
    var <- lookup name env
    evals <- mapM (`evalExpr` ret) es
    let evals' = case evals of
                    [] -> []
                    _ -> Prelude.map head (init evals) ++ last evals
    case var of
        Builtin (BuiltinCall c) -> do 
            out <- c evals'
            pure $ ret out
        Function closure params body -> do
            env <- get
            env' <- bindAll closure (zip params (evals' ++ repeat Nil))
            put env'
            mapM_ (`evalStmt` ret) (getStatements body)
            put env
            pure $ ret []
        _ -> throwError $ NotCallable var

evalStmt (Do b) ret = evalBlock b ret

evalStmt (While e b) ret = do
    undefined

evalStmt Dummy ret = pure $ ret []

isTrue :: Value -> Bool
isTrue Nil = False
isTrue (Boolean False) = False
isTrue _ = True

evalExpr :: Expr -> ([Value] -> [Value]) -> Eval [Value]
evalExpr (EValue a) ret = pure $ ret [a]

evalExpr (EVar var) ret = do
    env <- get
    val <- lookup var env
    pure $ ret [val]

evalExpr (ECall name args) ret = do 
    env <- get
    var <- lookup name env
    evals <- mapM (`evalExpr` ret) args
    let evals' = case evals of
                    [] -> []
                    _ -> Prelude.map head (init evals) ++ last evals
    case var of
        Builtin (BuiltinCall c) -> do 
            out <- c evals'
            pure $ ret out
        Function closure params body -> do
            env <- get
            env' <- bindAll closure (zip params (evals' ++ repeat Nil))
            put env'
            vals <- mapM (`evalStmt` ret) (getStatements body)
            put env
            pure $ ret (luaFlattenList vals)
        _ -> throwError $ NotCallable var -- lookup function and call it
evalExpr (EFuncDef params block) ret = do
    closure <- get
    pure $ ret [Function closure params block]

evalExpr (EPar e) ret = evalExpr e ret

evalExpr (EBinOp And e1 e2) ret = do
    e1t' <- evalExpr e1 ret
    let e1' = head e1t'
    if isTrue e1' then evalExpr e2 ret else pure $ ret [e1']

evalExpr (EBinOp Or e1 e2) ret = do
    e1t' <- evalExpr e1 ret
    let e1' = head e1t'
    if isTrue e1' then pure $ ret [e1'] else evalExpr e2 ret

evalExpr (EBinOp op e1 e2) ret = do
    e1t' <- evalExpr e1 ret
    let e1' = head e1t'
    e2t' <- evalExpr e2 ret
    let e2' = head e2t'
    res <- case op of
                Add -> add e1' e2'
                Sub -> sub e1' e2'
                Mul -> mul e1' e2'
                Div -> div' e1' e2'
                BitwiseAnd -> band e1' e2'
                BitwiseOr -> bor e1' e2'
                Xor -> xor' e1' e2'
                Eq -> eq' e1' e2'
                Lt -> e1' `lt` e2'
                Gt -> e2' `lt` e1'
                Ge -> e2' `le` e1'
                Le -> e1' `le` e2'
                Ne -> e1' `neq'` e2'
                Concat -> e1' `concat'` e2'
    pure $ ret $ (:[]) res

evalExpr (EUnOp op a) ret = do
    at' <- evalExpr a ret
    let a' = head at'
    res <- case op of
        Minus -> minus' a'
        Not -> bitwiseNot' a'
        Len -> len' a'
    pure $ ret $ (:[]) res
    

unOp :: (Numeric -> Numeric) -> String -> Value -> Eval Value
unOp op _ (Number a) = pure $ Number $ op a
unOp _ opStr a = throwError (UnsupportedUnOperation opStr (EValue a))

eq' :: Value -> Value -> Eval Value
eq' a b = pure $ Boolean $ a == b
neq' :: Value -> Value -> Eval Value
neq' a b = pure $ Boolean $ a /= b
concat' :: Value -> Value -> Eval Value
concat' a b = pure $ String $ show a ++ show b
len' :: Value -> Eval Value
len' (String s) = pure $ Number $ fromIntegral (length s)
len' i = throwError (UnsupportedUnOperation "length" (EValue i))
minus' = unOp negate "unary minus"
bitwiseNot' = unOp bitwiseNot "bitwise negation"

lt :: Value -> Value -> Eval Value
lt (String a) (String b) = pure $ Boolean $ a < b
lt (Number a) (Number b) = pure $ Boolean $ a < b
lt a b = throwError (UnsupportedBinOperation "less-than" (EValue a) (EValue b))

le :: Value -> Value -> Eval Value
le (String a) (String b) = pure $ Boolean $ a <= b
le (Number a) (Number b) = pure $ Boolean $ a <= b
le a b = throwError (UnsupportedBinOperation "less-than-or-equals" (EValue a) (EValue b))

binOp :: (Numeric -> Numeric -> Numeric) -> String -> Value -> Value -> Eval Value
binOp op _ (Number a) (Number b) = pure $ Number (a `op` b)
binOp _ opStr a b = throwError (UnsupportedBinOperation opStr (EValue a) (EValue b))

add = binOp (+) "addition"
sub = binOp (-) "subtraction"
mul = binOp (*) "multiplication"
div' = binOp (/) "division"
band = binOp bitwiseAnd "bitwise and"
bor = binOp bitwiseOr "bitwise or"
xor' = binOp bitwiseXor "xor"
