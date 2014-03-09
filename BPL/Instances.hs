{-# LANGUAGE StandaloneDeriving #-}

module BPL.Instances
       ()
       where

import Control.Applicative
import Control.Monad

import BPL.Types

instance Monad Parser where
  return x = Parser $ \ts -> Right $ Just (x, ts)

  x >>= f = Parser $ \ts -> do
    result <- runParser x ts
    case result of
      Nothing -> Right Nothing
      Just (r, ts') -> runParser (f r) ts'

  fail err = Parser $ \state -> case state of
    [] -> Left $ "parse error at end of file : " ++ err
    (Token _ _ line:_) -> Left $ "parse error at line: " ++ show line ++ " : " ++ err

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Alternative Parser where
  empty = Parser $ \_ -> Right Nothing
  p <|> q = Parser $ \ts -> case runParser p ts of
    Left err -> Left err
    Right Nothing -> runParser q ts
    right -> right

instance Functor Parser where
  fmap f x = do
    result <- x
    return (f result)

instance Show Token where
  show (Token t v l) = show t ++ " \"" ++ v ++ "\"" ++ " : (line " ++ show l ++ ")"

deriving instance Show Var

indent :: Int -> String
indent n = concat $ replicate n "| "

class ShowIndent a where
  showIndent :: Int -> a -> String

instance ShowIndent Expr where
  showIndent n (CompExp left op right) = indent n
                                         ++ show op
                                         ++ "\n"
                                         ++ showIndent (n+1) left
                                         ++ showIndent (n+1) right
  showIndent n (ArithExp left op right) = indent n
                                          ++ show op
                                          ++ "\n"
                                          ++ showIndent (n+1) left
                                          ++ showIndent (n+1) right
  showIndent n (IntExp i) = indent n ++ show i ++ "\n"
  showIndent n (StringExp s) = indent n ++ s ++ "\n"
  showIndent n (VarExp ref) = indent n ++ ref ++ "\n"
  showIndent n (DerefExp ref) = indent n ++ "Deref " ++ ref ++ "\n"
  showIndent n (AddrExp ref) = indent n ++ "Address " ++ ref ++ "\n"
  showIndent n (ArrayExp ref index) = indent n
                                      ++ "ArrayExp "
                                      ++ ref
                                      ++ "\n"
                                      ++ showIndent (n+1) index
  showIndent n (FuncExp func args) = indent n
                                     ++ "FunCall "
                                     ++ func
                                     ++ "\n"
                                     ++ concatMap (showIndent (n+1)) args
  showIndent n ReadExp = indent n ++ "ReadExp\n"
  showIndent n (AssignExp ref exp) = indent n
                                     ++ "Assign "
                                     ++ show ref
                                     ++ "\n"
                                     ++ showIndent (n+1) exp

instance Show Expr where
  show = showIndent 0

instance ShowIndent Statement where
  showIndent n stmt = case stmt of
    WriteLnStmt -> indent n ++ "WriteLnStmt\n"
    WriteStmt expr -> indent n ++ "WriteStmt\n" ++ showIndent (n+1) expr
    CompoundStmt decs statements -> indent n
                                    ++ "CompoundStmt\n"
                                    ++ concatMap (showIndent (n+1)) decs
                                    ++ concatMap (showIndent (n+1)) statements
    ExpressionStmt expr -> showIndent n expr
    IfStmt expr stmt -> indent n
                        ++ "IfStmt\n"
                        ++ showIndent (n+1) expr
                        ++ showIndent (n+1) stmt
    IfElseStmt expr ifStmt elseStmt -> indent n
                                       ++ "IfElseStmt\n"
                                       ++ showIndent (n+1) expr
                                       ++ showIndent (n+1) ifStmt
                                       ++ showIndent (n+1) elseStmt
    WhileStmt expr stmt -> indent n
                           ++ "While\n"
                           ++ showIndent (n+1) expr
                           ++ showIndent (n+1) stmt
    ReturnStmt expr -> indent n
                       ++ "Return\n"
                       ++ case expr of
                         Just e -> showIndent (n+1) e
                         Nothing -> ""

instance Show Statement where
  show = showIndent 0

instance ShowIndent VarDec where
  showIndent n (VarDec typeSpec s) = indent n
                                     ++ "VarDec\n"
                                     ++ indent (n+1)
                                     ++ show typeSpec
                                     ++ "\n"
                                     ++ indent (n+1)
                                     ++ s
                                     ++ "\n"
  showIndent n (PointerDec typeSpec s) = indent n
                                         ++ "PointerDec\n"
                                         ++ indent (n+1)
                                         ++ show typeSpec
                                         ++ "\n"
                                         ++ indent (n+1)
                                         ++ s
                                         ++ "\n"
  showIndent n (ArrayDec typeSpec s l) = indent n
                                         ++ "Arraydec\n"
                                         ++ indent (n+1)
                                         ++ show typeSpec
                                         ++ "\n"
                                         ++ indent (n+1)
                                         ++ s
                                         ++ "\n"
                                         ++ indent (n+1)
                                         ++ show l
                                         ++ "\n"

instance Show VarDec where
  show = showIndent 0

instance ShowIndent FunDec where
  showIndent n (FunDec typeSpec ref decls stmt) = indent n
                                                  ++ show typeSpec
                                                  ++ " "
                                                  ++ ref
                                                  ++ "\n"
                                                  ++ indent (n+1)
                                                  ++ "Args:\n"
                                                  ++ concatMap (showIndent (n+2)) decls
                                                  ++ indent (n+1)
                                                  ++ "Body:\n"
                                                  ++ showIndent (n+2) stmt

instance Show FunDec where
  show = showIndent 0

instance Show Declaration where
  show (FDecl d) = showIndent 0 d
  show (VDecl d) = showIndent 0 d
