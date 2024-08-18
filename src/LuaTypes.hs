{-# LANGUAGE InstanceSigs #-}
module LuaTypes where

import qualified Data.Int
import Data.Map
import Prelude hiding (lookup, showList)
import System.Environment ()
import Data.List

-- TODO: object store? Address -> Value
type Identifier = String

data Env

-- AST is based on https://www.lua.org/manual/5.4/manual.html#9
data Numeric
  = Integer Data.Int.Int64
  | Double Double

instance Show Numeric where
  show (Integer i) = show i
  show (Double d) = show d

instance Ord Numeric where
  Integer a <= Integer b = a <= b
  Integer a <= Double b = fromIntegral a <= b
  Double a <= Integer b = a <= fromIntegral b
  Double a <= Double b = a <= b

instance Eq Numeric where
  Integer a == Integer b = a == b
  Integer a == Double b = fromIntegral a == b
  Double a == Integer b = a == fromIntegral b
  Double a == Double b = a == b

data Table = TableData
  { getTable :: Map Value Value
  , getMetatable :: Maybe Table
  } deriving (Eq, Ord)


data Value = Value
  { getReferenceId :: Maybe Int
  , getValue :: Type
  } deriving (Ord)

data Type
  = Nil
  | Number Numeric
  | Boolean Bool
  | String String
  | Function
      { getEnv :: () -- TODO
      , getArguments :: [Int]
      , getBody :: Block
      }
  | Table Table
  -- | Userdata
  -- | Thread
  deriving (Eq, Ord)

instance Show Type where
  show Nil = "nil"
  show (Number n) = show n
  show (Boolean b) = show b
  show (String s) = show s
  show _ = undefined

instance Eq Value where
  (==) :: Value -> Value -> Bool
  (Value _ Nil) == (Value _ Nil) = True
  (Value _ (Number v1)) == (Value _ (Number v2)) = v1 == v2
  (Value _ (Boolean b1)) == (Value _ (Boolean b2)) = b1 == b2
  (Value _ (String s1)) == (Value _ (String s2)) = s1 == s2
  (Value id1 (Function {})) == (Value id2 (Function {})) = id1 == id2
  (Value id1 (Table t1)) == (Value id2 (Table t2)) =
    case (t1, t2) of
      (TableData _ _, TableData _ _) -> id1 == id2
  _ == _ = False

data Lhs
  = LIdent Identifier
  | LIndex Expr Expr
  deriving (Eq, Ord)

instance Show Lhs where
  show (LIdent i) = i
  show (LIndex a b) = show a ++ "[" ++ show b ++ "]"

data Statement
  = Assignment [Lhs] [Expr]
  | Call Identifier [Expr]
  | Break
  | Do Block
  | While Expr Block
  | RepeatUntil Expr Block
  | If Expr Block [(Expr, Block)] (Maybe Block)
  | Local [Identifier] (Maybe [Expr])
  | ForNum Identifier Expr Expr (Maybe Expr) Block
  | ForIn [Identifier] [Expr] Block
  | Dummy
  -- | Label String
  -- | Goto String
  deriving (Eq, Ord)

commaSeparatedList :: [String] -> String
commaSeparatedList vals = intercalate ", " vals
showList l = Data.List.map show $ l
commaSeparatedShow vals = commaSeparatedList (showList vals)

indent :: String -> String
indent = concatMap (\x -> if x == '\n' then "\n" ++ tab else [x])

tab :: String
tab = "  "

instance Show Statement where
  show (Assignment lhs rhs) = commaSeparatedShow lhs ++ " = " ++ commaSeparatedShow rhs
  show (Call fnName es) = fnName ++ "(" ++ commaSeparatedShow es ++ ")"
  show Break = "break"
  show (Do b) = "do" ++ indent ("\n" ++ show b) ++ "\nend"
  show (While cond b) = "while " ++ show cond ++ " do" ++ indent ("\n" ++ show b) ++ "\nend"
  show (RepeatUntil cond b) = "repeat" ++ indent ("\n" ++ show b) ++ "\n" ++ "until " ++ show cond
  show (If ifcond tb elseifs elseb) = "if " ++ show ifcond ++ " then" ++ indent ("\n" ++ show tb)
    ++ intercalate "" (Data.List.map showElseIf elseifs) ++ (
      case elseb of
        Just b -> "else" ++ indent ("\n" ++ show b)
        Nothing -> ""
    ) ++ "\nend"
    where showElseIf (e, b) = "\nelseif " ++ show e ++ " then" ++ indent ("\n" ++ show b)
  show (Local ids vals) = "local " ++ commaSeparatedList ids ++ (
    case vals of
      Just v -> " = " ++ commaSeparatedShow v
      Nothing -> ""
    )
  show (ForNum ident start end step b) = "for " ++ ident ++ "=" ++ show start ++ "," ++ show end ++ (
    case step of
      Just s -> "," ++ show s
      Nothing -> ""
    ) ++ " do" ++ indent ("\n" ++ show b) ++ "\nend"
  show (ForIn ids exps b) = "for " ++ commaSeparatedList ids ++ " in " ++ commaSeparatedShow exps ++ " do" ++ indent ("\n" ++ show b) ++ "\nend"
  show Dummy = ""

data Block = Block
  { getStatements :: [Statement]
  , getReturn :: Maybe [Expr]
  } deriving (Eq, Ord)

instance Show Block where
  show (Block s r) = intercalate "\n" (Data.List.map show s) ++ (
    case r of
      Just r' -> "\nreturn " ++ commaSeparatedShow r'
      Nothing -> ""
    )
data Expr
  = EType Type
  | EBinOp BinOp Expr Expr
  | EUnOp UnOp Expr
  | EVar Identifier
  | ECall Identifier [Expr]
  | EPar Expr
  | ELhs Lhs
  | EDots
  | EFuncDef [Identifier] Block
  deriving (Eq, Ord)

isAtomic :: Expr -> Bool
isAtomic (EType _) = True
isAtomic (EBinOp _ _ _) = False
isAtomic (EUnOp _ _) = True
isAtomic (EVar _) = True
isAtomic (ECall _ _) = True
isAtomic (EPar _) = True
isAtomic (ELhs _) = False
isAtomic EDots = True
isAtomic (EFuncDef _ _) = False

instance Show Expr where
  show (EType val) = show val
  show (EBinOp op e1 e2) = (
    if isAtomic e1 then show e1 else "(" ++ show e1 ++ ")"
    ) ++ " " ++ show op ++ " " ++(  if isAtomic e2 then show e2 else "(" ++ show e2 ++ ")")
  show (EUnOp op e) = show op ++ show e
  show (EVar i) = i
  show (ECall i es) = show i ++ "(" ++ commaSeparatedShow es ++ ")"
  show (EPar e) = "(" ++ show e ++ ")"
  show (ELhs l) = show l
  show EDots = "..."
  show (EFuncDef params b) = "function (" ++ commaSeparatedList params ++ ")" ++ indent ("\n" ++ show b) ++ "\nend"
data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | FloorDiv
  | Mod
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
  show FloorDiv = "//"
  show Mod = "%"
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