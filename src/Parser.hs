{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Parser where

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

-- | I don't know why but I got overlapping instances with Control.Monad.Trans.Error
-- I started getting the error after including both Control.Monad.Except and Control.Monad.State .
-- Hopefully the {-# OVERLAPS -#} doesn't break anything.
instance {-# OVERLAPS #-} Alternative (Either ParserError) where
  empty = Left $ ParserError 1 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1


instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = ws *> charP' x <* ws

charP' :: Char -> Parser Char
charP' x = Parser f
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
stringP str = ws *> stringP' str <* ws

stringP' :: String -> Parser String
stringP' str =
  Parser $ \input ->
    case runParser (traverse charP' str) input of
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

eof :: Parser Char
eof = Parser $ \input ->
  case inputHead input of
    Just (y,_) -> Left $ ParserError
                    (inputLoc input)
                    ("Expected end of file, but found '" ++ [y] ++ "'")
    Nothing -> Right (' ', input)

parseIf' :: String -> (Char -> Bool) -> Parser Char
parseIf' desc f =
  Parser $ \input ->
    case input of
      (inputHead -> Just (y, ys))
        | f y -> Right (y, ys)
        | otherwise ->
          Left
            $ ParserError
                (inputLoc input)
                ("Unexpected symbol: '" ++ [y] ++ "'")
      _ ->
        Left
          $ ParserError
              (inputLoc input)
              ("Expected " ++ desc ++ ", but reached end of string")

parseIf :: String -> (Char -> Bool) -> Parser Char
parseIf desc f = ws *> parseIf' desc f <* ws

spanP :: String -> (Char -> Bool) -> Parser [Char]
spanP desc = many . parseIf' desc

span1P :: String -> (Char -> Bool) -> Parser String
span1P desc = some . parseIf' desc

ws :: Parser String
ws = many (parseIf' "whitespace character" isSpace <|> (' ' <$ shortComment))

luaBlock :: Parser Block
luaBlock = (\x y-> case y of
                    Just y' -> Block (x ++ [Return y'])
                    Nothing -> Block x) <$> many luaStmt <*> optional retStmt

retStmt :: Parser [Expr]
retStmt = stringP "return" *> (fromMaybe [] <$> optional explist1) <* optional (charP ';')

luaStmt :: Parser Statement
luaStmt =
  luaAssignment
    <|> funcCallStmt
    <|> localAssignment
    <|> dummy
    <|> break
    <|> doBlock
    <|> ifStmt
    <|> namedFunc

namedFunc :: Parser Statement
namedFunc =
  (\name vars block -> Assignment [name] [EFuncDef vars block])
    <$> (stringP "function" *> luaIdentifier)
    <*> (charP '(' *> identlist <* charP ')')
    <*> (luaBlock <* stringP "end")

localAssignment :: Parser Statement
localAssignment =
  Local
    <$> (stringP "local" *> identlist1)
    <*> optional (charP '=' *> explist1)

identlist :: Parser [Identifier]
identlist = sepBy (charP ',') luaIdentifier

identlist1 :: Parser [Identifier]
identlist1 = sepBy1 (charP ',') luaIdentifier

dummy :: Parser Statement
dummy = Dummy <$ stringP ";"

ifStmt :: Parser Statement
ifStmt = If <$> if' <*> then' <*> elseifs <*> optional else' <* stringP "end"
  where
    if' = stringP "if" *> luaExpr
    then' = stringP "then" *> luaBlock
    elseifs = many $ (,) <$> (stringP "elseif" *> luaExpr) <*> then'
    else' = stringP "else" *> luaBlock


doBlock :: Parser Statement
doBlock = Do <$> (stringP "do" *> luaBlock <* stringP "end")

break :: Parser Statement
break = Break <$ stringP "break"

luaExpr :: Parser Expr
luaExpr = expr0

luaAtom :: Parser Expr
luaAtom =
  luaValue
    <|> funcCallExpr
    <|> luaVar
    <|> unnamedFunc
    <|> (charP '(' *> luaExpr <* charP ')')

funcCallExpr :: Parser Expr
funcCallExpr = ECall <$>
            (ws *> luaIdentifier) <*> (charP '(' *> explist <* charP ')')
funcCallStmt :: Parser Statement
funcCallStmt = Call <$>
            (ws *> luaIdentifier) <*> (charP '(' *> explist <* charP ')')

unnamedFunc :: Parser Expr
unnamedFunc =
  EFuncDef
    <$> (stringP "function"
           *> charP '('
           *> identlist
           <* charP ')')
    <*> (luaBlock <* stringP "end")

luaValue :: Parser Expr
luaValue = luaNil <|> luaNumber <|> luaBool <|> luaString

sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep element = (:) <$> element <*> many (sep *> element)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = sepBy1 sep element <|> pure []

luaAssignment :: Parser Statement
luaAssignment =
  (Assignment . map (\(EVar y) -> y)
     <$> (ws *> varlist <* charP '='))
    <*> explist1
  where
    varlist = sepBy1 (charP ',') luaVar

explist :: Parser [Expr]
explist = sepBy (charP ',') luaExpr

explist1 :: Parser [Expr]
explist1 = sepBy1 (charP ',') luaExpr

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
        <$> parseIf' "start of identifier" isAlphaOrUnderscore
        <*> spanP "part of identifier" isAlphaNum

luaNil :: Parser Expr
luaNil = EValue Nil <$ stringP "nil"

luaBool :: Parser Expr
luaBool = luaTrue <|> luaFalse
  where
    luaTrue = EValue (Boolean True) <$ stringP "true"
    luaFalse = EValue (Boolean False) <$ stringP "false"

luaNumber :: Parser Expr
luaNumber = EValue . Number <$> (numberHex <|> numberDec)

luaString :: Parser Expr
luaString =
  EValue . String <$> (charP '"' *> spanP "quote" (/= '"') <* charP '"')

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
    <*> optional (charP' '.' *> option 0 digits1) -- fractional part
    <*> optional
          ((charP' 'e' <|> charP' 'E') *> ((*) <$> optSign <*> option 0 digits1))

numberHex :: Parser Numeric
numberHex =
  partsToNum 16 2
    <$> optMinus -- sign 
    <* (ws <* (stringP' "0x" <|> stringP' "0X"))
    <*> hexDigits1 -- decimal part
    <*> optional (charP' '.' *> option 0 hexDigits1) -- fractional part
    <*> optional
          ((charP' 'p' <|> charP' 'P')
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
shortComment = stringP' "--" *> ("" <$ spanP "non-newline character" (/= '\n'))

expr0 = chainl1 expr1 $ EBinOp Or <$ stringP "or"

expr1 = chainl1 expr2 $ EBinOp And <$ stringP "and"

expr2 =
  chainl1 expr3
    $ EBinOp
        <$> ((Le <$ stringP "<=")
               <|> (Ge <$ stringP ">=")
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
               <|> (Div <$ stringP "/"))

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
