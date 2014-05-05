module Main
       where

import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
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
    Right decls -> case flip runState (M.empty, 0) . runWriterT . runMaybeT . mapM checkDecl . fst . createSymbolTable $ decls of
      ((Just ast, output), (table, _)) -> putStrLn output >> putStrLn "A well-typed program!" >> print table
      ((Nothing, output), _) -> putStrLn output
