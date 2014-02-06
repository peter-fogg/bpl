module Scanner
       where

data TokenType = Identifier
               | Number
               | Int
               | Void
               | String
               | If
               | Else
               | While
               | Return
               | Write
               | WriteLn
               | Read
               | Semicolon
               | Comma
               | LSquare
               | RSquare
               | LCurly
               | RCurly
               | LParen
               | RParen
               | RAngle
               | LAngle
               | LEQ
               | GEQ
               | Equal
               | NotEqual
               | Plus
               | Minus
               | Star
               | Slash
               | Percent
               | Ampersand
               | EOF
               deriving (Eq, Show)

main = do
  putStrLn $ show Identifier
