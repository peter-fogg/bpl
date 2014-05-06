module Main
       where

import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import System.Environment (getArgs)

import BPL.Check
import BPL.DSL
import BPL.Generate
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
    Right decls -> case flip runState (M.empty, 0) . runWriterT . runMaybeT . mapM checkDecl $ decls' of
      ((Just ast, output), (table, _)) -> let code = generateCode $ genBPL decls' table
                                          in putStrLn code >> writeFile "out.s" code
      ((Nothing, output), _) -> putStrLn output
      where decls' = fst . createSymbolTable $ decls
