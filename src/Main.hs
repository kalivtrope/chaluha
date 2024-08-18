{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Main where
import LuaTypes
import Parsing
import System.Environment
import Control.Applicative


main :: IO ()
main = do
    content <- getContents
    print (runParser (luaBlock <* ws <* eof) $ Input 1 content)