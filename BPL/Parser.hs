module BPL.Parser
       (
         parseBPL
       )
       where

import Control.Applicative
import qualified Data.Map as M

import BPL.Types

parseBPL = do
  decls <- some declaration
  consume TkEOF <|> fail "expected EOF or declaration"
  return decls

endParse :: Parser a
endParse = Parser $ \_ -> Right Nothing

consume :: TokenType -> Parser ()
consume typ = Parser $ \(t:ts) -> Right $
                                  if tokenType t == typ
                                  then Just ((), ts)
                                  else  Nothing

wrap :: TokenType -> TokenType -> Parser a -> Parser a
wrap l r p = do
  consume l
  result <- p
  consume r <|> fail ("missing " ++ show r)
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
  Right (Just (result, ts')) -> Right $ Just (Just result, ts')
  Right Nothing -> Right $ Just (Nothing, ts)
  Left err -> Left err

number :: Parser (Expr ())
number = Parser $ \(t:ts) -> case t of
  Token TkNumber n _ -> Right $ Just (IntExp $ read n, ts)
  _ -> Right Nothing

string :: Parser (Expr ())
string = Parser $ \(t:ts) -> case t of
  Token TkStringLiteral s _ -> Right $ Just (StringExp s, ts)
  _ -> Right Nothing

dataType :: Parser TypeSpecifier
dataType = Parser $ \(t:ts) -> case t of
  Token TkInt _ _ -> Right $ Just (TInt, ts)
  Token TkString _ _ -> Right $ Just (TString, ts)
  Token TkVoid _ _ -> Right $ Just (TVoid, ts)
  _ -> Right Nothing

identifier :: Parser String
identifier = Parser $ \(t:ts) -> case t of
  Token TkIdentifier s _ -> Right $ Just (s, ts)
  _ -> Right Nothing

declaration :: Parser (Declaration ())
declaration = do
  func <- parseMaybe funDec
  case func of
    Just f -> return $ FDecl f
    Nothing -> do
      var <- parseMaybe localDec
      case var of
        Just v -> return $ VDecl v
        Nothing -> endParse

param :: Parser VarDec
param = do
  t <- dataType <|> fail eNoType
  star <- parseMaybe $ consume TkStar
  ident <- identifier <|> fail eNoIdnt
  arr <- parseMaybe $ squares $ return ()
  case (star, arr) of
    (Nothing, Nothing) -> return $ VarDec t ident
    (Nothing, Just _) -> return $ ArrayDec t ident 0 -- We'll never use the length
    (Just _, Nothing) -> return $ PointerDec t ident
    (_, _) -> fail "attempt to declare pointer to array"

funDec :: Parser (FunDec ())
funDec = do
  typ <- dataType
  name <- identifier
  params <- parens $ (consume TkVoid >> return []) <|> sep TkComma param
  body <- compoundStmt
  return $ FunDec typ name params body

localDec :: Parser VarDec
localDec = do
  t <- dataType
  star <- parseMaybe $ consume TkStar
  ident <- identifier
  len <- parseMaybe $ squares number
  consume TkSemicolon <|> fail eNoSemi
  case (star, len) of
    (Nothing, Nothing) -> return $ VarDec t ident
    (Nothing, Just (IntExp l)) -> return $ ArrayDec t ident l
    (Just _, Nothing) -> return $ PointerDec t ident
    (_, _) -> fail "attempt to declare pointer to array"

expression :: Parser (Expr ())
expression = assignExp
             <|> derefExp
             <|> addrExp
             <|> readExp
             <|> compExp
             <|> string

var :: Parser (Var ())
var = do
  star <- parseMaybe $ consume TkStar
  ref <- identifier
  index <- parseMaybe $ squares expression
  return $ case (star, index) of
    (Nothing, Nothing) -> IdVar ref ()
    (Nothing, Just e) -> ArrVar ref e ()
    (Just _, Nothing) -> DerefVar ref ()

assignExp :: Parser (Expr ())
assignExp = do
  ref <- var
  consume TkSingleEqual
  val <- expression <|> fail eNoExpr
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
    Just op -> Right $ Just (op, ts)
    Nothing -> Right Nothing

relOp :: Parser RelOp
relOp = operator relOps

addOp :: Parser ArithOp
addOp = operator addOps

mulOp :: Parser ArithOp
mulOp = operator mulOps

derefExp :: Parser (Expr ())
derefExp = do
  consume TkStar
  ref <- identifier <|> fail (eNoIdnt ++ " after * operator")
  return $ DerefExp ref ()

addrExp :: Parser (Expr ())
addrExp = do
  consume TkAmpersand
  ref <- identifier <|> fail (eNoIdnt ++ " after & operator")
  return $ AddrExp ref ()

arrayExp :: Parser (Expr ())
arrayExp = do
  ref <- identifier
  index <- squares expression
  return $ ArrayExp ref index ()

varExp :: Parser (Expr ())
varExp = fmap (flip VarExp ()) identifier

funcExp :: Parser (Expr ())
funcExp = do
  ident <- identifier
  args <- parens $ sep TkComma expression <|> return []
  return $ FuncExp ident args ()

readExp :: Parser (Expr ())
readExp = do
  consume TkRead
  parens (return ()) <|> fail "malformed read()"
  return ReadExp

factor :: Parser (Expr ())
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

eExp :: Parser (Expr ())
eExp = infixParser addOp tExp ArithExp

tExp :: Parser (Expr ())
tExp = infixParser mulOp factor ArithExp

compExp :: Parser (Expr ())
compExp = infixParser relOp eExp CompExp

infixParser :: Parser a -> Parser (Expr ()) -> (Expr () -> a -> Expr () -> Expr ()) -> Parser (Expr ())
infixParser opType exprType c = (first >>= rest) <|> first <|> exprType
  where first = exprType >>= rest
        rest left = do
          op <- opType
          right <- exprType
          return $ c left op right

statement :: Parser (Statement ())
statement = compoundStmt
            <|> ifStmt
            <|> whileStmt
            <|> returnStmt
            <|> writeStmt
            <|> writeLnStmt
            <|> expressionStmt

compoundStmt :: Parser (Statement ())
compoundStmt = curlies $ do
  decls <- many localDec
  stmts <- many statement
  return $ CompoundStmt decls stmts

expressionStmt :: Parser (Statement ())
expressionStmt = do
  expr <- expression
  consume TkSemicolon <|> fail eNoSemi
  return $ ExpressionStmt expr

ifStmt :: Parser (Statement ())
ifStmt = do
  consume TkIf
  cond <- parens expression <|> fail eNoCond
  stmt <- statement <|> fail eNoBody
  els <- parseMaybe elseStmt
  return $ case els of
    Nothing -> IfStmt cond stmt
    Just s -> IfElseStmt cond stmt s

elseStmt :: Parser (Statement ())
elseStmt = do
  consume TkElse
  statement <|> fail eNoBody

whileStmt :: Parser (Statement ())
whileStmt = do
  consume TkWhile
  cond <- parens expression <|> fail eNoCond
  stmt <- statement <|> fail eNoBody
  return $ WhileStmt cond stmt

returnStmt :: Parser (Statement ())
returnStmt = do
  consume TkReturn
  expr <- parseMaybe expression
  consume TkSemicolon <|> fail eNoSemi
  return $ ReturnStmt expr

writeStmt :: Parser (Statement ())
writeStmt = do
  consume TkWrite
  expr <- parens expression <|> fail eNoBody
  consume TkSemicolon <|> fail eNoSemi
  return $ WriteStmt expr

writeLnStmt :: Parser (Statement ())
writeLnStmt = do
  consume TkWriteLn
  parens (return ())
  consume TkSemicolon <|> fail eNoSemi
  return WriteLnStmt

errorString :: String -> Token -> String
errorString expected (Token t v l) = "expected " ++ expected
                                     ++ ", found " ++ show t
                                     ++ " with value " ++ v
                                     ++ " on line " ++ show l

eNoBody = "missing body"
eNoCond = "missing condition"
eNoExpr = "missing expression"
eNoSemi = "missing semicolon"
eNoIdnt = "missing identifier"
eNoType = "missing data type"
