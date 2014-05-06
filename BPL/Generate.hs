module BPL.Generate
       where

import Control.Monad
import qualified Data.Map as M

import BPL.DSL
import BPL.Types

writeHeader :: CodeGen ()
writeHeader = do
  write ".WriteIntString: .string \"%d \""
  write ".WriteStringString: .string \"%s\""
  write ".WriteLnString: .string \"\\n\""
  write ".ReadIntString: .string \"%d\""
  write ".text"
  write ".globl main"

writeStmt :: Label -> CodeGen ()
writeStmt addr = do
  movl eax esi # "put argument where C expects is"
  movq addr rdi # "put format string in %esi"
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

genExpr :: M.Map String String -> Expr SymbolTable -> CodeGen ()
genExpr t (StringExp s) = case M.lookup s t of
  Nothing -> error "string wasn't assigned a label"
  Just l -> movl ("$"++l) eax
genExpr _ _ = return ()

genStmt :: M.Map String String -> Statement SymbolTable -> CodeGen ()
genStmt t (CompoundStmt _ stmts) = mapM_ (genStmt t) stmts
genStmt t (WriteStmt expr) = genExpr t expr >> writeStmt writeStringString
genStmt _ WriteLnStmt = writeStmt writeLnString
genStmt _ _ = return ()

genDecl :: M.Map String String -> Declaration SymbolTable -> CodeGen ()
genDecl t (FDecl (FunDec _ fname _ stmt)) = do
  let varLength = localVarLength 0 stmt
  fname -: do
    movq rsp rbx # "move stack pointer to frame pointer"
    sub (($.)varLength) rsp # "allocate local vars"
    genStmt t stmt
    addq (($.)varLength) rsp # "deallocate local vars"
    when (fname == "main") $ movl (($.)0) eax # "main should return 0"
    ret
genDecl _ (VDecl (VarDec _ i name)) = write $ ".comm " ++ name ++ ", " ++ show i ++ ", 32"

allocateStrings :: M.Map String String -> CodeGen ()
allocateStrings t = forM_ (M.toAscList t) $ \(s, l) -> do
  write $ l ++ ": .string " ++ show s

genBPL :: [Declaration SymbolTable] -> M.Map String String -> CodeGen ()
genBPL decls t = do
  mapM_ (genDecl t) (reverse vdecls)
  write ".section\t.rodata"
  allocateStrings t
  writeHeader
  mapM_ (genDecl t) (reverse fdecls)
  where (vdecls, fdecls) = foldl go ([], []) decls
        go (vs, fs) f@(FDecl _) = (vs, f:fs)
        go (vs, fs) v@(VDecl _) = (v:vs, fs)

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
