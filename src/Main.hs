{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Main where
import Parser
import Eval (runEvalDefault, evalBlock)


main :: IO ()
main = do
    content <- getContents
    let parseResult = runParser (luaBlock <* ws <* eof) $ Input 1 content
    case parseResult of
        Left e -> print e
        Right val -> do
             --print $ fst val
            evalResult <- runEvalDefault (evalBlock (fst val) id)
            case evalResult of
                Left err -> print err
                Right _ -> pure ()
    
