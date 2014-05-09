module BPL.Generate
       where

import Control.Monad
import qualified Data.Map as M

import BPL.DSL
import BPL.Types

writeHeader :: CodeGen ()
writeHeader = do
  write ".WriteIntString: .string \"%lld \""
  write ".WriteStringString: .string \"%s\""
  write ".WriteLnString: .string \"\\n\""
  write ".ReadIntString: .string \"%lld\""
  write ".text"
  write ".globl main"

writeStmt :: Label -> CodeGen ()
writeStmt addr = do
  movq rax rsi # "put argument where C expects it"
  movq addr rdi # "put format string in %esi"
  movq (($.)0) rax
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
genExpr t (CompExp l op r) = do
  trueLabel <- label
  falseLabel <- label
  genExpr t l
  push rax
  genExpr t r
  cmpq (0 rsp) rax # "comparison"
  case op of
    OpLeq -> jg falseLabel
    OpLe -> jge falseLabel
    OpEq -> jne falseLabel
    OpNeq -> je falseLabel
    OpGe -> jle falseLabel
    OpGeq -> jl falseLabel
  movq (($.)1) rax # "put 1 in accumulator if true"
  jmp trueLabel # "jump to true label"
  falseLabel -: movq (($.)0) rax # "put 0 in accumulator if false"
  trueLabel -: add (($.)8) rsp # "pop stack"
genExpr t (ArithExp l op r) = do
  genExpr t l # "generate left side"
  push rax
  genExpr t r # "generate right side"
  case op of
    OpPlus -> addq (0 rsp) rax # "add the two operands"
    OpMinus -> do
      sub rax (0 rsp) # "subtract the two operands"
      movq (0 rsp) rax # "put value in accumulator"
    OpTimes -> imul (0 rsp) rax # "multiply the two operands"
    _ -> do
      movq rax rbp # "put divisor in rbp"
      movq (0 rsp) rax # "put dividend into rax"
      cqto
      idiv rbp # "wat"
  when (op == OpMod) $ movq rdx rax # "put remainder into accumulator"
  addq (($.)8) rsp # "pop the stack"
genExpr _ (IntExp i) = movq (($.)i) rax # "load number"
genExpr t (StringExp s) = case M.lookup s t of
  Nothing -> error "string wasn't assigned a label"
  Just l -> movq ("$"++l) rax
genExpr t ReadExp = do
  movq (($.)0) rax # "clear return value"
  sub (($.)40) rsp # "decrement stack pointer for read()"
  leaq (24 rsp) rsi # "pointer for read() result"
  movq readIntString rdi # "move format string into rdi"
  call scanf
  movq (24 rsp) rax # "put result in accumulator"
  addq (($.)40) rsp # "pop stack"
genExpr _ _ = return ()

genStmt :: M.Map String String -> Statement SymbolTable -> CodeGen ()
genStmt t (CompoundStmt _ stmts) = mapM_ (genStmt t) stmts
genStmt t (IfStmt e s) = do
  l <- label
  genExpr t e
  cmpq (($.)0) rax # "compare result to 0"
  je l
  genStmt t s
  l -: return ()
genStmt t (IfElseStmt e s1 s2) = do
  els <- label
  fin <- label
  genExpr t e
  cmpq (($.)0) rax # "compare result to 0"
  je els # "jump to else case"
  genStmt t s1
  jmp fin # "jump to end of if/else"
  els -: return () # "else"
  genStmt t s2
  fin -: return () # "end if/else"
genStmt t (WhileStmt e s) = do
  end <- label
  start <- label
  start -: return ()
  genExpr t e
  cmpq (($.)0) rax # "compare result to 0"
  je end # "jump over statement"
  genStmt t s
  jmp start # "loop"
  end -: return ()
genStmt t (ExpressionStmt e) = genExpr t e
-- TODO: this is wrong; array references to string arrays will fail (wrong format string)
genStmt t (WriteStmt expr) = genExpr t expr >> writeStmt (case expr of StringExp _ -> writeStringString
                                                                       _ -> writeIntString)
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
    when (fname == "main") $ movq (($.)0) rax # "main should return 0"
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
  write "\n"
  where (vdecls, fdecls) = foldl go ([], []) decls
        go (vs, fs) f@(FDecl _) = (vs, f:fs)
        go (vs, fs) v@(VDecl _) = (v:vs, fs)
