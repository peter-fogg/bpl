module BPL.Parser
       where

import Control.Applicative
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

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Alternative Parser where
  empty = Parser $ \ts -> Left ""
  p <|> q = Parser $ \ts -> case runParser p ts of
    Left err -> runParser q ts
    right -> right

instance Functor Parser where
  fmap f x = do
    result <- x
    return (f result)

-- What can we parse thus far? Eventually this should be the top-level
-- parser function.
parserThusFar = some localDec

consume :: TokenType -> Parser ()
consume typ = Parser $ \(t:ts) ->
  if tokenType t == typ
  then Right ((), ts)
  else Left $ errorString (show typ) t

wrap :: TokenType -> TokenType -> Parser a -> Parser a
wrap l r p = do
  consume l
  result <- p
  consume r
  return result

parens :: Parser a -> Parser a
parens = wrap TkLParen TkRParen

squares :: Parser a -> Parser a
squares = wrap TkLSquare TkRSquare

curlies :: Parser a -> Parser a
curlies = wrap TkLCurly TkRCurly

parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe p = Parser $ \ts -> case runParser p ts of
  Right (result, ts') -> Right (Just result, ts')
  Left err -> Right (Nothing, ts)

number :: Parser Expr
number = Parser $ \(t:ts) -> case t of
  Token TkNumber n line -> Right $ (IntExp $ read n, ts)
  token -> Left $ errorString "integer" token

string :: Parser Expr
string = Parser $ \(t:ts) -> case t of
  Token TkStringLiteral s line -> Right $ (StringExp s, ts)
  token -> Left $ errorString "string" token

dataType :: Parser TypeSpecifier
dataType = Parser $ \(t:ts) -> case t of
  Token TkInt _ _ -> Right $ (TInt, ts)
  Token TkString _ _ -> Right $ (TString, ts)
  Token TkVoid _ _ -> Right $ (TVoid, ts)
  token -> Left $ errorString "type identifier" token

identifier :: Parser String
identifier = Parser $ \(t:ts) -> case t of
  Token TkIdentifier s _ -> Right $ (s, ts)
  token -> Left $ errorString "identifier" token

localDec :: Parser VarDec
localDec = do
  t <- dataType
  star <- parseMaybe $ consume TkStar
  ident <- identifier
  len <- parseMaybe $ squares number
  consume TkSemicolon
  return $ case (star, len) of
    (Nothing, Nothing) -> VarDec t ident
    (Nothing, Just (IntExp l))  -> ArrayDec t ident l
    (Just s, Nothing)  -> PointerDec t ident

errorString :: String -> Token -> String
errorString expected (Token t v l) = "expected " ++ expected
                                     ++ ", found " ++ show t
                                     ++ " with value " ++ v
                                     ++ " on line " ++ show l
