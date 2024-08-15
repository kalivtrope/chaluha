{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Data.Data
import qualified Data.Int
import Data.Map
import GHC.IO.Handle (hFlush)
import System.Environment ()
import System.IO (stdout)
import Prelude hiding (lookup)

type Identifier = String

-- AST is based on https://www.lua.org/manual/5.4/manual.html#9
data Numeric
  = Integer Data.Int.Int64
  | Double Double
  deriving (Show)

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
  } deriving (Show, Eq, Ord)


data Value = Value
  { getReferenceId :: Maybe Int
  , getValue :: Type
  } deriving (Show, Ord)

data Type
  = Nil
  | Number Numeric
  | Boolean Bool
  | String String
  | Function
      { getEnv :: () -- TODO
      , getArgs :: [Int]
      , getBody :: Block
      }
  | Table Table
  -- | Userdata
  -- | Thread
  deriving (Show, Eq, Ord)

instance Eq Value where
  (==) :: Value -> Value -> Bool
  (Value _ Nil) == (Value _ Nil) = True
  (Value _ (Number v1)) == (Value _ (Number v2)) = v1 == v2
  (Value _ (Boolean b1)) == (Value _ (Boolean b2)) = b1 == b2
  (Value _ (String s1)) == (Value _ (String s2)) = s1 == s2
  (Value id1 (Function {})) == (Value id2 (Function {})) = id1 == id2
  (Value id1 (Table t1)) == (Value id2 (Table t2)) =
    case (t1, t2) of
      (TableData r1 mt1, TableData r2 mt2) -> (id1 == id2) -- TODO
  _ == _ = False

data Lhs
  = LIdent Identifier
  | LIndex Expr Expr
  deriving (Show, Eq, Ord)

data Statement
  = Assignment [Lhs] [Expr]
  | Call Identifier [Expr]
  | Break
  | Do Block
  | While Expr Block
  | RepeatUntil Expr Block
  | If Expr Block (Maybe [(Expr, Block)]) (Maybe Block)
  | Local [Identifier] (Maybe [Expr])
  | ForNum Expr Expr (Maybe Expr) Block
  | ForIn [Identifier] [Expr] Block
  -- | Label String
  -- | Goto String
  deriving (Show, Eq, Ord)

data Block = Block
  { getStatements :: [Statement]
  , getReturn :: Maybe [Expr]
  } deriving (Show, Eq, Ord)

data Expr
  = EValue Value
  | EBinOp BinOp Expr Expr
  | EUnOp UnOp Expr
  | EVar Identifier
  | ECall Identifier [Expr]
  | EPar Expr
  | ELhs Lhs
  | EDots
  deriving (Show, Eq, Ord)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | FloorDiv
  | Pow
  | Mod
  | Concat
  | Eq
  | Lt
  | Le
  | And
  | Or
  | BitwiseAnd
  | BitwiseOr
  | Xor
  | Lshift
  | Rshift

data UnOp
  = Minus
  | Not
  | Len
  | Neg
main :: IO ()
main = undefined
