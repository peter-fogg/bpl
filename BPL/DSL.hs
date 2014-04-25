module BPL.DSL
       where

import Control.Monad.State
import Data.List (intercalate)
import qualified Text.Printf as T

import BPL.Types

label :: CodeGen Label
label = do
  n <- fmap labels get
  modify (\c -> c { labels = succ n })
  return $ ".L" ++ show n

infixr 0 -:
(-:) :: Label -> CodeGen a -> CodeGen a
l -: c = write (l ++ ":") >> c

($.) :: Int -> String
($.) n = "$" ++ show n

write :: String -> CodeGen ()
write s = modify (\c -> c { code = code c ++ [s]})

gen :: OpCode -> String -> String -> CodeGen ()
gen op src dest = write $ T.printf "\t%s %s, %s" op src dest

add, addl, addq, sub, imul, movq, movl, cmpl :: Op
add = gen "add"
addl = gen "addl"
addq = gen "addq"
sub = gen "sub"
imul = gen "imul"
movq = gen "movq"
movl = gen "movl"
cmpl = gen "cmpl"

idivl :: String -> CodeGen ()
idivl s = write $ "\tidivl " ++ s

cltq, cqto :: CodeGen ()
cltq = write "\tcltq"
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

writeHeader :: CodeGen ()
writeHeader = do
  write ".section .rodata"
  write ".WriteIntString: .string \"%d \""
  write ".WriteStringString: .string \"%s\""
  write ".WriteLnString: .string \"\\n\""
  write ".ReadIntString: .string \"%d\""
  write ".text"
  write ".globl main"

testCode :: CodeGen ()
testCode = do
  writeHeader
  let f = "f"
  f -: do
    movq rsp rbx
    movq (16 rbx) rax
    imul (($.)2) eax
    ret
  let main = "main"
  main -: do
    movq rsp rbx
    sub (($.)8) rsp
    movl (($.)0) eax
    movl eax ((-8) rbx)
  l0 <- label
  l1 <- label
  l0 -: do
    cmpl (($.)10) ((-8) rbx)
    jge l1
    movl ((-8) rbx) esi
    movq writeIntString rdi
    call printf
    push ((-8) rbx)
    call f
    pop rbx
    add (($.)8) rsp
    movl eax esi
    movq writeIntString rdi
    movl (($.)0) eax
    call printf
    movq writeLnString rdi
    movl (($.)0) eax
    call printf
    movl ((-8) rbx) eax
    addl (($.)1) eax
    movl eax ((-8)rbx)
    jmp l0
  l1 -: do
    add (($.)8) rsp
    ret

generateCode :: CodeGen () -> String
generateCode cg = intercalate "\n" c
  where (CodeGenState _ c) = execState cg initialState
        initialState = CodeGenState 0 []

rax, eax, rsp, esp, rbx, ebx, rsi, esi, rdi, edi :: Register
rax = "%rax"
eax = "%eax"
rsp = "%rsp"
esp = "%esp"
rbx = "%rbx"
ebx = "%ebx"
rsi = "%rsi"
esi = "%esi"
rdi = "%rdi"
edi = "%edi"

writeIntString, writeStringString, writeLnString, readIntString, printf, scanf :: Label
writeIntString = "$.WriteIntString"
writeStringString = "$.WriteStringString"
writeLnString = "$.WriteLnString"
readIntString = "$.ReadIntString"
printf = "printf"
scanf = "scanf"
