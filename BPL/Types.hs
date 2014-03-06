module BPL.Types
       where
       -- (TokenType(..)
       -- , LineNumber
       -- , Token(..)
       -- , IntLiteral
       -- ) where

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
               deriving (Eq, Show)

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

data Expr = CompExp Expr RelOp Expr
          | ArithExp Expr ArithOp Expr
          | IntExp Int
          | StringExp String
          | VarExp String
          | PointerExp String
          | ArrayExp String Expr
          | FuncExp String [Expr]
          | ReadExp
          deriving (Show, Eq)
