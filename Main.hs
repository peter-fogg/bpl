module Main
       where

import System.Environment (getArgs)
import Control.Monad (forM_)

import BPL.Scanner
import BPL.Parser

main :: IO ()
main = do
  args <- getArgs
  let testFile = case args of
        (fname:_) -> fname
        _ -> "parser_test.bpl"
  contents <- readFile testFile
  case tokenize contents >>= runParser parserThusFar of
    Left err -> putStrLn $ "PROBLEMTOWN: " ++ err
    Right (decls, _) -> putStr $ concatMap show decls
