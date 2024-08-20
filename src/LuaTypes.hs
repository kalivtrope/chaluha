{-# LANGUAGE InstanceSigs #-}

module LuaTypes where

import Control.Monad.Except
import Control.Monad.State
import Data.Bits (Bits(..))
import Data.Foldable (foldrM)
import Data.IORef
import Data.Int (Int64)
import Data.List hiding (lookup)
import qualified Data.Map as Map
import Prelude hiding (lookup, showList, toInteger)
import System.Environment ()
import Data.Maybe (isNothing)

type Identifier = String

-- AST based on https://www.lua.org/manual/5.4/manual.html#9
data Numeric
  = Integer Data.Int.Int64
  | Double Double

instance Show Numeric where
  show (Integer i) = show i
  show (Double d) = show d

toDouble :: Numeric -> Numeric
toDouble (Double d) = Double d
toDouble (Integer i) = Double $ fromIntegral i

toInteger :: Numeric -> Numeric
toInteger (Double d) = Integer $ floor d
toInteger (Integer i) = Integer i

samEValue :: Numeric -> Numeric -> Bool
samEValue (Double _) (Double _) = True
samEValue (Integer _) (Integer _) = True
samEValue _ _ = False

instance Ord Numeric where
  Integer a <= Integer b = a <= b
  Double a <= Double b = a <= b
  x <= y = toDouble x <= toDouble y

instance Num Numeric where
  Integer a + Integer b = Integer (a + b)
  Double a + Double b = Double (a + b)
  x + y = toDouble x + toDouble y
  Integer a * Integer b = Integer (a * b)
  Double a * Double b = Double (a * b)
  x * y = toDouble x * toDouble y
  abs (Integer a) = Integer $ abs a
  abs (Double a) = Double $ abs a
  signum (Integer a) = Integer $ signum a
  signum (Double a) = Double $ signum a
  fromInteger i = Integer $ fromInteger i
  negate (Integer a) = Integer $ negate a
  negate (Double a) = Double $ negate a

bitwiseOp :: (Int64 -> Int64 -> Int64) -> Numeric -> Numeric -> Numeric
bitwiseOp op (Integer a) (Integer b) = Integer $ a `op` b
bitwiseOp op x y = bitwiseOp op (toInteger x) (toInteger y)

bitwiseNot (Integer a) = Integer $ complement a
bitwiseNot d = bitwiseNot $ toInteger d

bitwiseOr = bitwiseOp (.|.)

bitwiseAnd = bitwiseOp (.&.)

bitwiseXor = bitwiseOp xor

instance Fractional Numeric where
  fromRational :: Rational -> Numeric
  fromRational x = Double $ fromRational x
  (/) :: Numeric -> Numeric -> Numeric
  Double a / Double b = Double $ a / b
  x / y = toDouble x / toDouble y

instance Eq Numeric where
  Integer a == Integer b = a == b
  Integer a == Double b = fromIntegral a == b
  Double a == Integer b = a == fromIntegral b
  Double a == Double b = a == b

newtype BuiltinCall =  BuiltinCall ([Value] -> Eval [Value])
instance Eq BuiltinCall where
  _ == _ = False

data Value
  = Nil
  | Number Numeric
  | Boolean Bool
  | String String
  | Function
      { getEnv :: Env
      , getParams :: [Identifier]
      , getBody :: Block
      }
  | Builtin BuiltinCall
  deriving (Eq)

instance Show Value where
  show Nil = "nil"
  show (Number n) = show n
  show (Boolean b) = show b
  show (String s) = show s
  show (Builtin _) = "builtin"
  show (Function {}) = "function"

type Lhs
  = String
  -- | LIndex Expr Expr

data Statement
  = Assignment [Lhs] [Expr]
  | Call Identifier [Expr]
  | Break
  | Return [Expr]
  | Do Block
  | While Expr Block
  | RepeatUntil Expr Block
  | If Expr Block [(Expr, Block)] (Maybe Block)
  | Local [Identifier] (Maybe [Expr])
  | Dummy
  -- | Label String
  -- | Goto String
  deriving (Eq)

commaSeparatedList :: [String] -> String
commaSeparatedList = intercalate ", "

showList :: Show a => [a] -> [String]
showList = Data.List.map show

commaSeparatedShow :: Show a => [a] -> String
commaSeparatedShow vals = commaSeparatedList (showList vals)

indent :: String -> String
indent =
  concatMap
    (\x ->
       if x == '\n'
         then "\n" ++ tab
         else [x])

tab :: String
tab = "  "

instance Show Statement where
  show (Assignment lhs rhs) =
    commaSeparatedList lhs ++ " = " ++ commaSeparatedShow rhs
  show (Call fnName es) = fnName ++ "(" ++ commaSeparatedShow es ++ ")"
  show Break = "break"
  show (Do b) = "do" ++ indent ("\n" ++ show b) ++ "\nend"
  show (While cond b) =
    "while " ++ show cond ++ " do" ++ indent ("\n" ++ show b) ++ "\nend"
  show (RepeatUntil cond b) =
    "repeat" ++ indent ("\n" ++ show b) ++ "\n" ++ "until " ++ show cond
  show (If ifcond tb elseifs elseb) =
    "if "
      ++ show ifcond
      ++ " then"
      ++ indent ("\n" ++ show tb)
      ++ intercalate "" (Data.List.map showElseIf elseifs)
      ++ (case elseb of
            Just b -> "else" ++ indent ("\n" ++ show b)
            Nothing -> "")
      ++ "\nend"
    where
      showElseIf (e, b) =
        "\nelseif " ++ show e ++ " then" ++ indent ("\n" ++ show b)
  show (Local ids vals) =
    "local "
      ++ commaSeparatedList ids
      ++ (case vals of
            Just v -> " = " ++ commaSeparatedShow v
            Nothing -> "")
  show Dummy = ""
  show (Return e) = "return " ++ commaSeparatedShow e

newtype Block = Block
  { getStatements :: [Statement]
  } deriving (Eq)

instance Show Block where
  show (Block s) = intercalate "\n" (Data.List.map show s)

data Expr
  = EValue Value
  | EBinOp BinOp Expr Expr
  | EUnOp UnOp Expr
  | EVar Identifier
  | ECall Identifier [Expr]
  | EPar Expr
  | EFuncDef [Identifier] Block
  deriving (Eq)

isAtomic :: Expr -> Bool
isAtomic (EValue _) = True
isAtomic (EBinOp {}) = False
isAtomic (EUnOp _ _) = True
isAtomic (EVar _) = True
isAtomic (ECall _ _) = True
isAtomic (EPar _) = True
isAtomic (EFuncDef _ _) = False

instance Show Expr where
  show (EValue val) = show val
  show (EBinOp op e1 e2) =
    (if isAtomic e1
       then show e1
       else "(" ++ show e1 ++ ")")
      ++ " "
      ++ show op
      ++ " "
      ++ (if isAtomic e2
            then show e2
            else "(" ++ show e2 ++ ")")
  show (EUnOp op e) = show op ++ show e
  show (EVar i) = i
  show (ECall i es) = i ++ "(" ++ commaSeparatedShow es ++ ")"
  show (EPar e) = "(" ++ show e ++ ")"
  show (EFuncDef params b) =
    "function ("
      ++ commaSeparatedList params
      ++ ")"
      ++ indent ("\n" ++ show b)
      ++ "\nend"

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Concat
  | Eq
  | Lt
  | Gt
  | Ge
  | Le
  | Ne
  | And
  | Or
  | BitwiseAnd
  | BitwiseOr
  | Xor
  deriving (Eq, Ord)

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Concat = ".."
  show Eq = "=="
  show Lt = "<"
  show Gt = ">"
  show Ge = ">="
  show Le = "<="
  show Ne = "~="
  show And = "and"
  show Or = "or"
  show BitwiseAnd = "&"
  show BitwiseOr = "|"
  show Xor = "~"

data UnOp
  = Minus
  | Not
  | Len
  deriving (Eq, Ord)

instance Show UnOp where
  show Minus = "-"
  show Not = "~"
  show Len = "#"

data LuaError
  = UnsupportedBinOperation String Expr Expr
  | UnsupportedUnOperation String Expr
  | NotCallable Value
  deriving (Eq)

instance Show LuaError where
  show err =
    "Error: "
      ++ case err of
           UnsupportedBinOperation opStr v1 v2 ->
             "unsupported operands for '"
               ++ opStr
               ++ "': "
               ++ show v1
               ++ " "
               ++ show v2
           UnsupportedUnOperation opStr v -> 
                "unsupported operand for '"
                ++ opStr
                ++ "': "
                ++ show v
           NotCallable v -> "'" ++ show v ++ "' is not callable"

data Env = Env
  { bindings :: Map.Map String (IORef Value)
  , parent :: Maybe Env
  } deriving (Eq)

type Eval a = StateT Env (ExceptT LuaError IO) a

emptyEnv :: IO Env
emptyEnv = pure $ Env {bindings = Map.empty, parent = Nothing}

defaultEnv :: IO Env
defaultEnv = do
  builtinRefs <- mapM newIORef builtins
  pure (Env {bindings = builtinRefs, parent = Nothing})

-- guaranteed local assignment
bind :: String -> Value -> Env -> Eval Env
bind name value env = do
  valueRef <- liftIO $ newIORef value
  pure $ env {bindings = Map.insert name valueRef (bindings env)}


-- (potentially) global assignment
assign :: String -> Value -> Env -> Eval ()
assign name value env@(Env bindings' parent') =
  case Map.lookup name bindings' of
    Nothing ->
      case parent' of
        Nothing ->
          if value == Nil
            then do
              pure ()
            else do
              bound <- bind name value env
              put bound
        Just p -> do
          assign name value p
          env' <- get
          put env {parent = Just env'}
    Just valueRef ->
          liftIO $ writeIORef valueRef value

dumpEnv :: Env -> IO ()
dumpEnv (Env bindings' parent') =
  do
    print $ Map.keys (bindings')
    case parent' of
      Nothing -> pure ()
      Just p -> dumpEnv p

lookup :: String -> Env -> Eval Value
lookup name (Env bindings' parent') =
  case Map.lookup name bindings' of
    Nothing ->
      case parent' of
        Nothing -> pure Nil
        Just p -> lookup name p
    Just valueRef -> do
      liftIO $ readIORef valueRef

bindAll :: Env -> [(Identifier, Value)] -> Eval Env
bindAll = foldrM (uncurry bind)

builtins :: Map.Map String Value
builtins = Builtin . BuiltinCall <$> Map.fromList
  [
    ("print", printVal)
  ]
printVal :: [Value] -> Eval [Value]
printVal vals = do
  liftIO $ putStrLn $ intercalate tab (map show vals)
  pure []