module BPL.Generate
       where

import BPL.DSL
import BPL.Types

writeHeader :: CodeGen ()
writeHeader = do
  write ".section .rodata"
  write ".WriteIntString: .string \"%d \""
  write ".WriteStringString: .string \"%s\""
  write ".WriteLnString: .string \"\\n\""
  write ".ReadIntString: .string \"%d\""
  write ".text"
  write ".globl main"

writeInteger :: CodeGen ()
writeInteger = do
  movl eax esi
  movq writeIntString rdi
  movl (($.)0) eax
  call printf

writeString :: String -> CodeGen ()
writeString addr = do
  movl addr esi
  movq writeStringString rdi
  movl (($.)0) eax
  call printf

writeLn :: CodeGen ()
writeLn = do
  movq writeLnString rdi
  movl (($.)0) eax
  call printf

localVarLength :: Int -> Statement a -> Int
localVarLength i stmt = case stmt of
  CompoundStmt decls stmts -> foldl max i' (map (localVarLength i') stmts)
    where i' = i + sum (map (\(VarDec _ i _) -> i) decls)
  IfStmt _ ifStmt -> localVarLength i ifStmt
  IfElseStmt _ ifStmt elseStmt -> max (localVarLength i ifStmt) (localVarLength i elseStmt)
  WhileStmt _ whileStmt -> localVarLength i whileStmt
  _ -> i

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
