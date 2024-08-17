{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Parsing where

import Control.Applicative
import Data.Char
import qualified Data.Int as Data.Integer
import Data.Maybe
import qualified GHC.Enum as Int64
import LuaTypes

type Int64 = Data.Integer.Int64

data Input = Input
  { inputLoc :: Int
  , inputStr :: String
  } deriving (Show, Eq)

inputHead :: Input -> Maybe (Char, Input)
inputHead (Input _ []) = Nothing
inputHead (Input loc (x:xs)) = Just (x, Input (loc + inc) xs)
  where
    inc =
      if x == '\n'
        then 1
        else 0

data ParserError =
  ParserError Int String
  deriving (Show)

newtype Parser a = Parser
  { runParser :: Input -> Either ParserError (a, Input)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) =
    Parser $ \input -> do
      (a', input') <- p input
      pure (f a', input')

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Right (x, input)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (f, input') <- p1 input
      (a, input'') <- p2 input'
      pure (f a, input'')

instance Alternative (Either ParserError) where
  empty = Left $ ParserError 1 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f input@(inputHead -> Just (y, ys))
      | y == x = Right (x, ys)
      | otherwise =
        Left
          $ ParserError
              (inputLoc input)
              ("Expected '" ++ [x] ++ "', but found '" ++ [y] ++ "'")
    f input =
      Left
        $ ParserError
            (inputLoc input)
            ("Expected '" ++ [x] ++ "', but reached end of string")

stringP :: String -> Parser String
stringP str =
  Parser $ \input ->
    case runParser (traverse charP str) input of
      Left _ ->
        Left
          $ ParserError
              (inputLoc input)
              ("Expected \""
                 ++ str
                 ++ "\", but found \""
                 ++ inputStr input
                 ++ "\'")
      result -> result

pIfNotq :: String -> Parser String -> Parser String -> Parser String
pIfNotq desc (Parser p) (Parser q) = Parser $ \input ->
  case q input of
    Left _ -> p input
    Right (a,_) -> Left $ ParserError (inputLoc input) ("Expected " ++ desc ++ ", but found '" ++ a ++ "'")

parseIf :: String -> (Char -> Bool) -> Parser Char
parseIf desc f =
  Parser $ \input ->
    case input of
      (inputHead -> Just (y, ys))
        | f y -> Right (y, ys)
        | otherwise ->
          Left
            $ ParserError
                (inputLoc input)
                ("Expected " ++ desc ++ ", but found '" ++ [y] ++ "'")
      _ ->
        Left
          $ ParserError
              (inputLoc input)
              ("Expected " ++ desc ++ ", but reached end of string")

spanP :: String -> (Char -> Bool) -> Parser String
spanP desc = many . parseIf desc

span1P :: String -> (Char -> Bool) -> Parser String
span1P desc = some . parseIf desc

ws :: Parser String
ws = spanP "whitespace character" isSpace

luaExpr :: Parser Expr
luaExpr = luaValue <|> luaVar

luaValue :: Parser Expr
luaValue = luaNil <|> luaNumber <|> luaBool <|> luaString

sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep element = (:) <$> element <*> many (sep *> element)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = sepBy1 sep element <|> pure []

-- luaAssignment :: Parser Expr
luaAssignment = (Assignment . map (\(EVar y) -> LIdent y) <$> (varlist <* ws <* charP '=')) <*> explist
  where varlist = sepBy1 (ws *> charP ',' <* ws) luaVar
        explist = sepBy1 (ws *> charP ',' <* ws) luaExpr

luaVar = luaIdentifier

keywords :: [String]
keywords = ["and", "break", "do", "else", "elseif", "end",
           "false", "for", "function", "goto", "if", "in", "local", "nil", "not",
            "or", "repeat", "return", "then", "true", "until", "while"]

keywordsP :: Parser String
keywordsP = foldr (\kw p -> p <|> stringP kw) empty keywords

isAlphaOrUnderscore :: Char -> Bool
isAlphaOrUnderscore c = isAlpha c || c == '_'

luaIdentifier :: Parser Expr
luaIdentifier = EVar <$> pIfNotq "identifier" bareIdentifier keywordsP
  where bareIdentifier = (:) <$> parseIf "start of identifier" isAlphaOrUnderscore <*> spanP "part of identifier" isAlphaNum

luaNil :: Parser Expr
luaNil = EType Nil <$ stringP "nil"

luaBool :: Parser Expr
luaBool = luaTrue <|> luaFalse
  where
    luaTrue = EType (Boolean True) <$ stringP "true"
    luaFalse = EType (Boolean False) <$ stringP "false"

luaNumber :: Parser Expr
luaNumber = EType . Number <$> (numberHex <|> numberDec)

luaString :: Parser Expr
luaString = EType . String <$> (charP '"' *> spanP "quote" (/= '"') <* charP '"')

option :: a -> Parser a -> Parser a
option defVal p = p <|> pure defVal

digits1 :: Parser Int64
digits1 = read <$> span1P "digit" isDigit

hexDigits1 :: Parser Int64
hexDigits1 = read . (\val -> '0' : 'x' : val) <$> span1P "hex digit" isHexDigit

minus :: Parser Int64
minus = (-1) <$ charP '-'

optMinus :: Parser Int64
optMinus = option 1 minus

plus :: Parser Int64
plus = 1 <$ charP '+'

optSign :: Parser Int64
optSign = option 1 (plus <|> minus)

numberDec :: Parser Numeric
numberDec =
  partsToNum 10 10
    <$> optMinus -- sign 
    <* ws
    <*> digits1 -- decimal part
    <*> optional (charP '.' *> option 0 digits1) -- fractional part
    <*> optional
          ((charP 'e' <|> charP 'E') *> ((*) <$> optSign <*> option 0 digits1))

numberHex :: Parser Numeric
numberHex =
  partsToNum 16 2
    <$> optMinus -- sign 
    <* (ws <* (stringP "0x" <|> stringP "0X"))
    <*> hexDigits1 -- decimal part
    <*> optional (charP '.' *> option 0 hexDigits1) -- fractional part
    <*> optional
          ((charP 'p' <|> charP 'P')
             *> ((*) <$> optSign <*> option 0 hexDigits1))

partsToNum ::
     Double -- fractional base
  -> Double -- exponent base
  -> Int64 -- sign 
  -> Int64 -- decimal part
  -> Maybe Int64 -- fractional part
  -> Maybe Int64 -- exponent part
  -> Numeric
partsToNum fbase ebase sign dec frac exp' =
  if isNothing frac && isNothing exp'
    then Integer $ sign * dec
    else Double res
  where
    combineFrac :: Int64 -> Maybe Int64 -> Double
    combineFrac a (Just b) =
      fromIntegral a
        + (fromIntegral b / (fbase ^^ fromIntegral (length (show b))))
    combineFrac a Nothing = fromIntegral a
    combineExp :: Double -> Maybe Int64 -> Double
    combineExp a (Just b) = a * ebase ^^ b
    combineExp a Nothing = a
    res :: Double
    res =
      combineExp (fromIntegral sign * combineFrac (fromIntegral dec) frac) exp'

shortComment = stringP "--" *> ("" <$ spanP "non-newline character" (/= '\n'))
