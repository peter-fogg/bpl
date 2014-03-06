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
            deriving (Show, Eq)

data FunDec = FunDec TypeSpecifier String [VarDec] Statement
            deriving (Show, Eq)

data Declaration = FDecl FunDec | VDecl VarDec
                 deriving (Show, Eq)

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

instance Show Expr where
  show e = showIndent 0 e ++ "\n"
    where showIndent n (CompExp left op right) = indent n
                                                 ++ show op
                                                 ++ "\n"
                                                 ++ showIndent (n+1) left ++ "\n"
                                                 ++ showIndent (n+1) right
          showIndent n (ArithExp left op right) = indent n
                                                  ++ show op
                                                  ++ "\n"
                                                  ++ showIndent (n+1) left ++ "\n"
                                                  ++ showIndent (n+1) right
          showIndent n (IntExp i) = indent n ++ show i
          showIndent n (StringExp s) = indent n ++ s
          showIndent n (VarExp ref) = indent n ++ ref
          showIndent n (DerefExp ref) = indent n ++ "Deref " ++ ref
          showIndent n (AddrExp ref) = indent n ++ "Address " ++ ref
          showIndent n (ArrayExp ref index) = indent n
                                              ++ "ArrayExp "
                                              ++ ref
                                              ++ "["
                                              ++ show index
                                              ++ "]"
          showIndent n (FuncExp func args) = indent n
                                             ++ "Function "
                                             ++ func
                                             ++ "\n"
                                             ++ concatMap (\arg -> showIndent (n+1) arg ++ "\n") args
          showIndent n ReadExp = indent n ++ "read()"
          showIndent n (AssignExp ref exp) = indent n
                                             ++ "Assign "
                                             ++ show ref
                                             ++ "\n"
                                             ++ showIndent (n+1) exp
          indent n = concat $ replicate n "| "

data Statement = CompoundStmt [VarDec] [Statement]
               | ExpressionStmt Expr
               | IfStmt Expr Statement
               | IfElseStmt Expr Statement Statement
               | WhileStmt Expr Statement
               | ReturnStmt (Maybe Expr)
               | WriteStmt Expr
               | WriteLnStmt
               deriving (Show, Eq)
