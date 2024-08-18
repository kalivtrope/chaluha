{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Parsing where

import Control.Applicative
import Data.Char
import qualified Data.Int as Data.Integer
import Data.Maybe
import LuaTypes
import Prelude hiding (break)

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
pIfNotq desc (Parser p) (Parser q) =
  Parser $ \input ->
    case q input of
      Left _ -> p input
      Right (a, _) ->
        Left
          $ ParserError
              (inputLoc input)
              ("Expected " ++ desc ++ ", but found '" ++ a ++ "'")

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
ws = many (parseIf "whitespace character" isSpace <|> (' ' <$ shortComment))

luaBlock :: Parser Block
luaBlock = Block <$> (ws *> many (luaStmt <* ws)) <*> optional retStmt

retStmt :: Parser [Expr]
retStmt = stringP "return" *> ws *> explist <* ws <* optional (charP ';')

luaStmt :: Parser Statement
luaStmt =
  luaAssignment
    <|> localAssignment
    <|> dummy
    <|> break
    <|> doBlock
    <|> whileBlock
    <|> repeatUntilBlock
    <|> ifStmt
    <|> forNum
    <|> forIn
    <|> namedFunc

namedFunc :: Parser Statement
namedFunc =
  (\name vars block -> Assignment [LIdent name] [EFuncDef vars block])
    <$> (stringP "function" *> ws *> luaIdentifier)
    <*> (charP '(' *> ws *> identlist <* ws <* charP ')' <* ws)
    <*> (luaBlock <* ws <* stringP "end")

forNum :: Parser Statement
forNum =
  ForNum
    <$> (stringP "for" *> ws *> luaExpr)
    <*> (ws *> luaExpr <* ws)
    <*> optional luaExpr
    <*> (ws *> luaBlock)

forIn :: Parser Statement
forIn =
  ForIn
    <$> (stringP "for" *> ws *> identlist <* ws)
    <*> (stringP "in" *> ws *> explist <* ws)
    <*> (stringP "do" *> ws *> luaBlock <* ws <* stringP "end")

localAssignment :: Parser Statement
localAssignment =
  Local
    <$> (stringP "local" *> ws *> identlist <* ws)
    <*> optional (charP '=' *> ws *> explist)

identlist :: Parser [Identifier]
identlist = sepBy1 (ws *> charP ',' <* ws) luaIdentifier

dummy :: Parser Statement
dummy = Dummy <$ stringP ";"

ifStmt :: Parser Statement
ifStmt = If <$> if' <*> then' <*> elseifs <*> optional else' <* stringP "end"
  where
    if' = stringP "if" *> ws *> luaExpr <* ws
    then' = stringP "then" *> ws *> luaBlock <* ws
    elseifs = many $ (,) <$> (stringP "elseif" *> ws *> luaExpr <* ws) <*> then'
    else' = stringP "else" *> ws *> luaBlock <* ws

repeatUntilBlock :: Parser Statement
repeatUntilBlock =
  RepeatUntil
    <$> (stringP "repeat" *> ws *> luaExpr <* ws)
    <*> (stringP "until" *> ws *> luaBlock <* stringP "end")

whileBlock :: Parser Statement
whileBlock =
  While
    <$> (stringP "while" *> ws *> luaExpr <* ws)
    <*> luaBlock
    <* stringP "end"

doBlock :: Parser Statement
doBlock = Do <$> (stringP "do" *> ws *> luaBlock <* stringP "end")

break :: Parser Statement
break = Break <$ stringP "break"

luaExpr :: Parser Expr
luaExpr = expr0

luaAtom :: Parser Expr
luaAtom =
  luaValue
    <|> luaVar
    <|> unnamedFunc
    <|> (charP '(' *> ws *> luaExpr <* ws <* charP ')')

unnamedFunc :: Parser Expr
unnamedFunc =
  EFuncDef
    <$> (stringP "function"
           *> ws
           *> charP '('
           *> ws
           *> identlist
           <* ws
           <* charP ')'
           <* ws)
    <*> (luaBlock <* ws <* stringP "end")

luaValue :: Parser Expr
luaValue = luaNil <|> luaNumber <|> luaBool <|> luaString

sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep element = (:) <$> element <*> many (sep *> element)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = sepBy1 sep element <|> pure []

luaAssignment :: Parser Statement
luaAssignment =
  (Assignment . map (\(EVar y) -> LIdent y)
     <$> (varlist <* ws <* charP '=' <* ws))
    <*> explist
  where
    varlist = sepBy1 (ws *> charP ',' <* ws) luaVar

explist :: Parser [Expr]
explist = sepBy1 (ws *> charP ',' <* ws) luaExpr

luaVar :: Parser Expr
luaVar = EVar <$> luaIdentifier

keywords :: [String]
keywords =
  [ "and"
  , "break"
  , "do"
  , "else"
  , "elseif"
  , "end"
  , "false"
  , "for"
  , "function"
  , "goto"
  , "if"
  , "in"
  , "local"
  , "nil"
  , "not"
  , "or"
  , "repeat"
  , "return"
  , "then"
  , "true"
  , "until"
  , "while"
  ]

keywordsP :: Parser String
keywordsP = foldr (\kw p -> p <|> stringP kw) empty keywords

isAlphaOrUnderscore :: Char -> Bool
isAlphaOrUnderscore c = isAlpha c || c == '_'

luaIdentifier :: Parser Identifier
luaIdentifier = pIfNotq "identifier" bareIdentifier keywordsP
  where
    bareIdentifier =
      (:)
        <$> parseIf "start of identifier" isAlphaOrUnderscore
        <*> spanP "part of identifier" isAlphaNum

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
luaString =
  EType . String <$> (charP '"' *> spanP "quote" (/= '"') <* charP '"')

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

shortComment :: Parser String
shortComment = stringP "--" *> ("" <$ spanP "non-newline character" (/= '\n'))

expr0 = chainl1 expr1 $ EBinOp Or <$ stringP "or"

expr1 = chainl1 expr2 $ EBinOp And <$ stringP "and"

expr2 =
  chainl1 expr3
    $ EBinOp
        <$> ((Le <$ stringP "<=")
               <|> (Ge <$ stringP ">=")
               <|> (Lt <$ stringP "<")
               <|> (Gt <$ stringP ">")
               <|> (Ne <$ stringP "~=")
               <|> (Lt <$ stringP "<")
               <|> (Gt <$ stringP ">")
               <|> (Eq <$ stringP "=="))

expr3 = chainl1 expr4 $ EBinOp BitwiseOr <$ charP '|'

expr4 = chainl1 expr5 $ EBinOp Xor <$ charP '~'

expr5 = chainl1 expr6 $ EBinOp BitwiseAnd <$ charP '&'

expr6 = chainr1 expr7 $ EBinOp Concat <$ stringP ".."

expr7 = chainl1 expr8 $ EBinOp <$> (Add <$ charP '+' <|> Sub <$ charP '-')

expr8 =
  chainl1 expr9
    $ EBinOp
        <$> ((Mul <$ charP '*')
               <|> (Div <$ stringP "/")
               <|> (Mod <$ stringP "%"))

expr9 =
  prefix luaAtom
    $ EUnOp
        <$> ((Not <$ stringP "not")
               <|> (Len <$ charP '#')
               <|> (Minus <$ charP '-'))

prefix :: Parser a -> Parser (a -> a) -> Parser a
prefix p op = (op <* ws) <*> prefix p op <|> p

infixl1 :: (a -> b) -> Parser a -> Parser (b -> a -> b) -> Parser b
infixl1 wrap p op = (wrap <$> p) <**> rest
  where
    rest = flip (.) <$> (flip <$> op <*> p) <*> rest <|> pure id

-- source: https://dl.acm.org/doi/pdf/10.1145/3471874.3472984
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = (p <* ws) <**> rest
  where
    rest =
      flip (.) <$> (flip <$> (ws *> op <* ws) <*> (ws *> p <* ws)) <*> rest
        <|> pure id

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op =
  (ws *> p <* ws) <**> (flip <$> (ws *> op <* ws) <*> chainr1 p op <|> pure id)
