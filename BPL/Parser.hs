module BPL.Parser
       where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M

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
parserThusFar = some statement

consume :: TokenType -> Parser ()
consume typ = Parser $ \(t:ts) ->
  if tokenType t == typ
  then Right ((), ts)
  else Left $ errorString (show typ) t

oneOf :: [TokenType] -> Parser TokenType
oneOf tokens = Parser $ \(t:ts) ->
  let typ = tokenType t in
  if typ `elem` tokens
  then Right (typ, ts)
  else Left $ "expected on of " ++ show tokens ++ ", found " ++ show typ

wrap :: TokenType -> TokenType -> Parser a -> Parser a
wrap l r p = do
  consume l
  result <- p
  consume r
  return result

sep :: TokenType -> Parser a -> Parser [a]
sep t p = do
  result <- p
  rest <- (consume t >> sep t p) <|> return []
  return $ result:rest

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

expression :: Parser Expr
expression = assignExp
             <|> derefExp
             <|> addrExp
             <|> readExp
             <|> compExp
             <|> string

var :: Parser Var
var = do
  star <- parseMaybe $ consume TkStar
  ref <- identifier
  index <- parseMaybe $ squares expression
  return $ case (star, index) of
    (Nothing, Nothing) -> IdVar ref
    (Nothing, Just e) -> ArrVar ref e
    (Just _, Nothing) -> DerefVar ref

assignExp :: Parser Expr
assignExp = do
  ref <- var
  consume TkSingleEqual
  val <- expression
  return $ AssignExp ref val

relOps :: M.Map TokenType RelOp
relOps = M.fromList [ (TkRAngle, OpGe)
                    , (TkLAngle, OpLe)
                    , (TkLEQ, OpGeq)
                    , (TkGEQ, OpLeq)
                    , (TkDoubleEqual, OpEq)
                    , (TkNotEqual, OpNeq)
                    ]

addOps :: M.Map TokenType ArithOp
addOps = M.fromList [ (TkPlus, OpPlus)
                    , (TkMinus, OpMinus)
                    ]

mulOps :: M.Map TokenType ArithOp
mulOps = M.fromList [ (TkStar, OpTimes)
                    , (TkSlash, OpDivide)
                    , (TkPercent, OpMod)
                    ]

operator :: M.Map TokenType a -> Parser a
operator map = Parser $ \(t:ts) ->
  case M.lookup (tokenType t) map of
    Just op -> Right (op, ts)
    Nothing -> Left $ errorString "binary operator" t

relOp :: Parser RelOp
relOp = operator relOps

addOp :: Parser ArithOp
addOp = operator addOps

mulOp :: Parser ArithOp
mulOp = operator mulOps

derefExp :: Parser Expr
derefExp = do
  consume TkStar
  ref <- identifier
  return $ DerefExp ref

addrExp :: Parser Expr
addrExp = do
  consume TkAmpersand
  ref <- identifier
  return $ AddrExp ref

arrayExp :: Parser Expr
arrayExp = do
  ref <- identifier
  index <- squares expression
  return $ ArrayExp ref index

varExp :: Parser Expr
varExp = fmap VarExp identifier

funcExp :: Parser Expr
funcExp = do
  ident <- identifier
  args <- parens $ (sep TkComma expression) <|> return []
  return $ FuncExp ident args

readExp :: Parser Expr
readExp = do
  consume TkRead
  consume TkLParen
  consume TkRParen
  return ReadExp

factor :: Parser Expr
factor = number
         <|> funcExp
         <|> derefExp
         <|> addrExp
         <|> arrayExp
         <|> varExp
         <|> parens expression
         <|> do
           consume TkMinus
           expr <- factor
           return $ ArithExp (IntExp (-1)) OpTimes expr

eExp :: Parser Expr
eExp = infixParser addOp tExp ArithExp

tExp :: Parser Expr
tExp = infixParser mulOp factor ArithExp

compExp :: Parser Expr
compExp = infixParser relOp eExp CompExp

infixParser :: Parser a -> Parser Expr -> (Expr -> a -> Expr -> Expr) -> Parser Expr
infixParser opType exprType c = (first >>= rest) <|> first <|> exprType
  where first = do
          left <- exprType
          op <- opType
          right <- exprType
          return $ c left op right
        rest left = do
          op <- opType
          right <- exprType
          return $ c left op right

statement :: Parser Statement
statement = compoundStmt
            <|> ifStmt
            <|> whileStmt
            <|> returnStmt
            <|> writeStmt
            <|> writeLnStmt
            <|> expressionStmt

compoundStmt :: Parser Statement
compoundStmt = curlies $ do
  decls <- many localDec
  stmts <- many statement
  return $ CompoundStmt decls stmts

expressionStmt :: Parser Statement
expressionStmt = do
  expr <- expression
  consume TkSemicolon
  return $ ExpressionStmt expr

ifStmt :: Parser Statement
ifStmt = do
  consume TkIf
  cond <- parens expression
  stmt <- statement
  els <- parseMaybe elseStmt
  return $ case els of
    Nothing -> IfStmt cond stmt
    Just s -> IfElseStmt cond stmt s

elseStmt :: Parser Statement
elseStmt = do
  consume TkElse
  statement

whileStmt :: Parser Statement
whileStmt = do
  consume TkWhile
  cond <- parens expression
  stmt <- statement
  return $ WhileStmt cond stmt

returnStmt :: Parser Statement
returnStmt = do
  consume TkReturn
  expr <- parseMaybe expression
  consume TkSemicolon
  return $ ReturnStmt expr

writeStmt :: Parser Statement
writeStmt = do
  consume TkWrite
  expr <- parens expression
  consume TkSemicolon
  return $ WriteStmt expr

writeLnStmt :: Parser Statement
writeLnStmt = do
  consume TkWriteLn
  parens (return ())
  consume TkSemicolon
  return WriteLnStmt

errorString :: String -> Token -> String
errorString expected (Token t v l) = "expected " ++ expected
                                     ++ ", found " ++ show t
                                     ++ " with value " ++ v
                                     ++ " on line " ++ show l
