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

data Parser a = Parser {
  runParser :: [Token] -> Either String (Maybe (a, [Token]))
  }

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
         deriving (Eq)

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

data Statement = CompoundStmt [VarDec] [Statement]
               | ExpressionStmt Expr
               | IfStmt Expr Statement
               | IfElseStmt Expr Statement Statement
               | WhileStmt Expr Statement
               | ReturnStmt (Maybe Expr)
               | WriteStmt Expr
               | WriteLnStmt
               deriving (Eq)
