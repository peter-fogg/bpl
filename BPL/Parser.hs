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

-- What can we parse thus far? Eventually this should be the top-level
-- parser function.
parserThusFar = many1 number

consume :: TokenType -> Parser ()
consume typ = Parser $ \(t:ts) ->
  if tokenType t == typ
  then Right ((), ts)
  else Left $ errorString (show typ) t

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser $ \ts -> case runParser p ts of
  Right result -> Right result
  Left err -> runParser q ts

many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest <- (many1 p) <|> return []
  return (first:rest)

number :: Parser IntLiteral
number = Parser $ \(t:ts) -> case t of
  Token TkNumber n line -> Right $ (IntLiteral $ read n, ts)
  token -> Left $ errorString "integer" token

string :: Parser StringLiteral
string = Parser $ \(t:ts) -> case t of
  Token TkStringLiteral s line -> Right $ (StringLiteral s, ts)
  token -> Left $ errorString "string" token

errorString :: String -> Token -> String
errorString expected (Token t v l) = "expected " ++ expected
                                     ++ ", found " ++ show t
                                     ++ " with value " ++ v
                                     ++ " on line " ++ show l
