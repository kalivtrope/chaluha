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

type Identifier = String

data Numeric
  = Integer Data.Int.Int64
  | Double Double

data Table = TableData
  { getTable :: Map Value Value
  , getMetatable' :: Table
  }

data Value = Value
  { getId :: Int
  , getValue :: Type
  , getMetatable :: Table
  }

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

instance Eq Value where
  (==) :: Value -> Value -> Bool
  (Value _ Nil _) == (Value _ Nil _) = True
  (Value _ (Number v1) _) == (Value _ (Number v2) _) = undefined
  (Value _ (Boolean b1) _) == (Value _ (Boolean b2) _) = b1 == b2
  (Value _ (String s1) _) == (Value _ (String s2) _) = s1 == s2
  (Value id1 (Function {}) mt1) == (Value id2 (Function {}) mt2) = (id1 == id2) -- TODO add support for __eq
  (Value id1 (Table t1) mt1) == (Value id2 (Table t2) mt2) = (id1 == id2) -- TODO
  _ == _ = False

-- instance Ord Value where

data Lhs
  = LIdent Identifier
  | LIndex Expr Expr

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
  | ForIn [(Identifier, Expr)] Block
  -- | Label String
  -- | Goto String

data Block = Block
  { getStatements :: [Statement]
  , getReturn :: Maybe [Expr]
  }

data Expr
  = EValue Value
  | EOp Op Expr Expr
  | ELhs Lhs
  | EDots

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Concat
  | Eq
  | Lt
  | Le
  | And
  | Or
  | Not
  | Len
main :: IO ()
main = undefined
