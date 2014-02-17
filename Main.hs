module Main
       where

import System.Environment (getArgs)
import Control.Monad (forM_)

import BPL.Scanner

main :: IO ()
main = do
  args <- getArgs
  let testFile = case args of
        (fname:_) -> fname
        _ -> "test.bpl"
  contents <- readFile testFile
  case tokenize contents of
    Left err -> putStrLn $ "PROBLEMTOWN: " ++ err
    Right tokens -> forM_ tokens (\t -> putStrLn (show t))
