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

data Var a = IdVar String | ArrVar String (Expr a) | DerefVar String
           deriving (Eq)

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
