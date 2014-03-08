module BPL.Types
       where

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

data TypeSpecifier = TInt | TString | TVoid deriving (Show, Eq)

data VarDec = VarDec TypeSpecifier String
            | PointerDec TypeSpecifier String
            | ArrayDec TypeSpecifier String Int
            deriving (Eq)

data FunDec = FunDec TypeSpecifier String [VarDec] Statement
            deriving (Eq)

data Declaration = FDecl FunDec | VDecl VarDec
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

data Var = IdVar String | ArrVar String Expr | DerefVar String
         deriving (Eq, Show)

data Expr = CompExp Expr RelOp Expr
          | ArithExp Expr ArithOp Expr
          | IntExp Int
          | StringExp String
          | VarExp String
          | DerefExp String
          | AddrExp String
          | ArrayExp String Expr
          | FuncExp String [Expr]
          | ReadExp
          | AssignExp Var Expr
          deriving (Eq)

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
                                     ++ concatMap (\arg -> showIndent (n+1) arg) args
  showIndent n ReadExp = indent n ++ "ReadExp\n"
  showIndent n (AssignExp ref exp) = indent n
                                     ++ "Assign "
                                     ++ show ref
                                     ++ "\n"
                                     ++ showIndent (n+1) exp

instance Show Expr where
  show = showIndent 0

data Statement = CompoundStmt [VarDec] [Statement]
               | ExpressionStmt Expr
               | IfStmt Expr Statement
               | IfElseStmt Expr Statement Statement
               | WhileStmt Expr Statement
               | ReturnStmt (Maybe Expr)
               | WriteStmt Expr
               | WriteLnStmt
               deriving (Eq)

instance ShowIndent Statement where
  showIndent n stmt = case stmt of
    WriteLnStmt -> indent n ++ "WriteLnStmt\n"
    WriteStmt expr -> indent n ++ "WriteStmt\n" ++ showIndent (n+1) expr
    CompoundStmt decs statements -> indent n
                                    ++ "CompoundStmt\n"
                                    ++ concatMap (\dec -> showIndent (n+1) dec) decs
                                    ++ concatMap (\smt -> showIndent (n+1) smt) statements
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
                                                  ++ concatMap (\d -> showIndent (n+2) d) decls
                                                  ++ indent (n+1)
                                                  ++ "Body:\n"
                                                  ++ showIndent (n+2) stmt

instance Show FunDec where
  show = showIndent 0

instance Show Declaration where
  show (FDecl d) = showIndent 0 d
  show (VDecl d) = showIndent 0 d
