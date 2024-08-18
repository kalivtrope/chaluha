{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Main where
import LuaTypes
import Parser
import System.Environment
import Control.Applicative


main :: IO ()
main = do
    content <- getContents
    let out = runParser (luaBlock <* ws <* eof) $ Input 1 content
    putStrLn (
        case out of
            Left e -> show e
            Right val -> show $ fst val
        )