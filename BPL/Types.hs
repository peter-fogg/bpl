module BPL.Types
       where

import Control.Applicative
import Control.Monad
import Data.Map as M

data TokenType = TkIdentifier
               | TkStringLiteral
               | TkNumber
               | TkString
               | TkInt
               | TkVoid
               | TkIf
               | TkElse
               | TkWhile
               | TkReturn
               | TkWrite
               | TkWriteLn
               | TkRead
               | TkSemicolon
               | TkComma
               | TkLSquare
               | TkRSquare
               | TkLCurly
               | TkRCurly
               | TkLParen
               | TkRParen
               | TkRAngle
               | TkLAngle
               | TkLEQ
               | TkGEQ
               | TkDoubleEqual
               | TkNotEqual
               | TkSingleEqual
               | TkPlus
               | TkMinus
               | TkStar
               | TkSlash
               | TkPercent
               | TkAmpersand
               | TkEOF
               deriving (Eq, Show, Ord)

type LineNumber = Integer

data Token = Token { tokenType :: TokenType
                   , value :: String
                   , line :: LineNumber
                   } deriving (Eq)

instance Show Token where
  show (Token t v l) = show t ++ " \"" ++ v ++ "\"" ++ " : (line " ++ show l ++ ")"

data Parser a = Parser {
  runParser :: [Token] -> Either String (Maybe (a, [Token]))
  }

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

data TypeSpecifier = TInt | TString | TVoid deriving (Show, Eq)

data VarDec = VarDec TypeSpecifier String
            | PointerDec TypeSpecifier String
            | ArrayDec TypeSpecifier String Int
            deriving (Eq)

data FunDec a = FunDec TypeSpecifier String [VarDec] (Statement a)
              deriving (Eq)

data Declaration a = FDecl (FunDec a) | VDecl VarDec
                   deriving (Eq)

data RelOp = OpLeq
           | OpLe
           | OpEq
           | OpNeq
           | OpGe
           | OpGeq
           deriving (Show, Eq)

data ArithOp = OpPlus
             | OpMinus
             | OpTimes
             | OpDivide
             | OpMod
             deriving (Show, Eq)

data Var a = IdVar String a | ArrVar String (Expr a) a | DerefVar String a
           deriving (Eq, Show)

data Expr a = CompExp (Expr a) RelOp (Expr a)
            | ArithExp (Expr a) ArithOp (Expr a)
            | IntExp Int
            | StringExp String
            | VarExp String a
            | DerefExp String a
            | AddrExp String a
            | ArrayExp String (Expr a) a
            | FuncExp String [(Expr a)] a
            | ReadExp
            | AssignExp (Var a) (Expr a)
            deriving (Eq)

data Statement a = CompoundStmt [VarDec] [Statement a]
                 | ExpressionStmt (Expr a)
                 | IfStmt (Expr a) (Statement a)
                 | IfElseStmt (Expr a) (Statement a) (Statement a)
                 | WhileStmt (Expr a) (Statement a)
                 | ReturnStmt (Maybe (Expr a))
                 | WriteStmt (Expr a)
                 | WriteLnStmt
                 deriving (Eq)

data SymbolTable = ST [(M.Map String (Declaration SymbolTable))] deriving (Show)

indent :: Int -> String
indent n = concat $ replicate n "| "

class ShowIndent a where
  showIndent :: Int -> a -> String

instance ShowIndent (Expr a) where
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
  showIndent n (VarExp ref _) = indent n ++ ref ++ "\n"
  showIndent n (DerefExp ref _) = indent n ++ "Deref " ++ ref ++ "\n"
  showIndent n (AddrExp ref _) = indent n ++ "Address " ++ ref ++ "\n"
  showIndent n (ArrayExp ref index _) = indent n
                                      ++ "ArrayExp "
                                      ++ ref
                                      ++ "\n"
                                      ++ showIndent (n+1) index
  showIndent n (FuncExp func args _) = indent n
                                     ++ "FunCall "
                                     ++ func
                                     ++ "\n"
                                     ++ concatMap (showIndent (n+1)) args
  showIndent n ReadExp = indent n ++ "ReadExp\n"
  showIndent n (AssignExp var exp) = indent n
                                     ++ "Assign "
                                     ++ showVar
                                     ++ showIndent (n+1) exp
    where showVar = case var of
            IdVar s _ -> s ++ "\n"
            ArrVar s e _ -> s ++ "[]\n" ++ showIndent (n+1) e
            DerefVar s _ -> s ++ "\n"

instance Show (Expr a) where
  show = showIndent 0

instance ShowIndent (Statement a) where
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

instance Show (Statement a) where
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

instance ShowIndent (FunDec a) where
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

instance Show (FunDec a) where
  show = showIndent 0

instance Show (Declaration a) where
  show (FDecl d) = showIndent 0 d
  show (VDecl d) = showIndent 0 d
