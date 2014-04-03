module Main
       where

import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import System.Environment (getArgs)

import BPL.Check
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
  case tokenize contents >>= extractParseResult . runParser parseBPL of
    Left err -> putStrLn $ "PROBLEMTOWN: " ++ err
    Right decls -> case runWriter . runMaybeT . mapM checkDecl . fst . createSymbolTable $ decls of
      (Just _, output) -> putStrLn output >> putStrLn "A well-typed program!"
      (Nothing, output) -> putStrLn output
