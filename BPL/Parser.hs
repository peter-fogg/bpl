module BPL.Parser
       where

import Control.Monad

import BPL.Types

data Parser a = Parser {
  runParser :: [Token] -> Either String (a, [Token])
  }

instance Monad Parser where
  return x = Parser $ \ts -> Right (x, ts)

  x >>= f = Parser $ \ts -> do
    (result, ts') <- runParser x ts
    runParser (f result) ts'

  fail err = Parser $ \state -> case state of
    [] -> Left $ "parse error at end of file : " ++ err
    ((Token _ _ line):ts) -> Left $ "parse error at line: " ++ show line ++ " : " ++ err

instance Functor Parser where
  fmap f x = do
    result <- x
    return (f result)
