module Main
       where

import System.Environment (getArgs)

import BPL.Parser
import BPL.Scanner
import BPL.Types

main :: IO ()
main = do
  args <- getArgs
  let testFile = case args of
        (fname:_) -> fname
        _ -> "parser_test.bpl"
  contents <- readFile testFile
  case tokenize contents >>= runParser parseBPL of
    Right (Just (decls, _)) -> putStr $ concatMap show decls
    Right Nothing -> putStrLn "PROBLEMTOWN: failed parse (no information available)"
    Left err -> putStrLn $ "PROBLEMTOWN: " ++ err
