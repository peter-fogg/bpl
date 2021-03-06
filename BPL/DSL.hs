module BPL.DSL
       where

import Control.Monad.State
import Data.List (intercalate)
import qualified Text.Printf as T

import BPL.Types

newLabel :: CodeGen Label
newLabel = do
  n <- fmap labels get
  modify (\c -> c { labels = succ n })
  return $ ".L" ++ show n

label :: Label -> CodeGen ()
label l = write (l ++ ":")

infixr 0 -:
(-:) :: Label -> CodeGen a -> CodeGen a
l -: c = write (l ++ ":") >> c

infixr 0 #
(#) :: CodeGen a -> String -> CodeGen ()
c # s = c >> modify (\c' -> case code c' of
                        x:xs -> c' { code = (x ++ tabs x ++ "# " ++ s):xs }
                        _ -> c')
        where tabs x = concat $ replicate (1 + ((40 - (length x + 8)) `quot` 8)) "\t"

infixr 8 %
(%) :: Int -> Register -> String
i % r = T.printf "%d(%s)" i r

($.) :: Int -> String
($.) n = "$" ++ show n

write :: String -> CodeGen ()
write s = modify (\c -> c { code = s:code c})

gen :: OpCode -> String -> String -> CodeGen ()
gen op src dest = write $ T.printf "\t%s %s, %s" op src dest

add, sub, imul, mov, cmp, lea :: Op
add = gen "add"
sub = gen "sub"
imul = gen "imul"
mov = gen "movq"
cmp = gen "cmpq"
lea = gen "leaq"

idiv :: String -> CodeGen ()
idiv s = write $ "\tidiv " ++ s

cqto :: CodeGen ()
cqto = write "\tcqto"

push :: String -> CodeGen ()
push s = write $ "\tpush " ++ s

pop :: String -> CodeGen ()
pop s = write $ "\tpop " ++ s

jmp :: Label -> CodeGen ()
jmp l = write $ "\tjmp " ++ l

jl, jle, jg, jge, jz, je, jne :: Label -> CodeGen ()
jl l = write $ "\tjl " ++ l
jle l = write $ "\tjle " ++ l
jg l = write $ "\tjg " ++ l
jge l = write $ "\tjge " ++ l
je l = write $ "\tje " ++ l
jne l = write $ "\tjne " ++ l
jz l = write $ "\tjz " ++ l

call :: Label -> CodeGen ()
call l = write $ "\tcall " ++ l

ret :: CodeGen ()
ret = write "\tret"

generateCode :: CodeGen () -> String
generateCode cg = intercalate "\n" $ reverse c
  where (CodeGenState _ c) = execState cg initialState
        initialState = CodeGenState 0 []

rax, rsp, rbx, rsi, rdi, rbp, rdx, r12 :: Register
rax = "%rax"
rsp = "%rsp"
rbx = "%rbx"
rsi = "%rsi"
rdi = "%rdi"
rbp = "%rbp"
rdx = "%rdx"
r12 = "%r12"

writeIntString, writeStringString, writeLnString, readIntString, printf, scanf :: Label
writeIntString = "$.WriteIntString"
writeStringString = "$.WriteStringString"
writeLnString = "$.WriteLnString"
readIntString = "$.ReadIntString"
printf = "printf"
scanf = "scanf"
