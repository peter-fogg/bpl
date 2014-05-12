module BPL.Generate
       where

import Control.Monad
import qualified Data.Map as M

import BPL.Check
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
  mov rax rsi # "put argument where C expects it"
  mov addr rdi # "put format string in %rdi"
  mov (($.)0) rax
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
genExpr t e = case e of
  CompExp l op r -> do
    trueLabel <- newLabel
    falseLabel <- newLabel
    genExpr t l
    push rax
    genExpr t r
    cmp rax (0 rsp) # "comparison"
    case op of
      OpLeq -> jg falseLabel
      OpLe -> jge falseLabel
      OpEq -> jne falseLabel
      OpNeq -> je falseLabel
      OpGe -> jle falseLabel
      OpGeq -> jl falseLabel
    mov (($.)1) rax # "put 1 in accumulator if true"
    jmp trueLabel # "jump to true label"
    falseLabel -: mov (($.)0) rax # "put 0 in accumulator if false"
    trueLabel -: add (($.)8) rsp # "pop stack"
  ArithExp l op r -> do
    genExpr t l
    push rax
    genExpr t r
    case op of
      OpPlus -> add (0 rsp) rax # "add the two operands"
      OpMinus -> do
        sub rax (0 rsp) # "subtract the two operands"
        mov (0 rsp) rax # "put value in accumulator"
      OpTimes -> imul (0 rsp) rax # "multiply the two operands"
      _ -> do
        mov rax rbx # "put divisor in rbx"
        mov (0 rsp) rax # "put dividend into rax"
        cqto
        idiv rbx # "wat"
    when (op == OpMod) $ mov rdx rax # "put remainder into accumulator"
    add (($.)8) rsp # "pop the stack"
  IntExp i -> mov (($.)i) rax # "load number"
  StringExp s -> case M.lookup s t of
    Nothing -> error "string wasn't assigned a label"
    Just l -> mov ("$"++l) rax
  VarExp s symTab -> case tableLookup symTab s of
    Nothing -> error "unbound symbol passed typechecking!"
    Just (_, Nothing, TIntArray _) -> mov ("$" ++ s) rax # "load global array"
    Just (_, Nothing, TStringArray _) -> mov ("$" ++ s) rax # "load global array"
    Just (_, Nothing, _) -> mov s rax # "load global variable"
    Just (_, Just i, _) -> mov (i % rbp) rax # "load local variable"
  DerefExp e' _ -> do
    genExpr t e'
    mov (0 rax) rax # "result of pointer dereference"
  AddrExp e' _ -> case e' of
    VarExp s symTab -> case tableLookup symTab s of
      Nothing -> error "unbound symbol passed typechecking!"
      Just (_, Nothing, _) -> lea s rax # "load address of global variable"
      Just (_, Just i, _) -> lea (i % rbp) rax # "load address of local variable"
    ArrayExp s idx symTab -> do
      genExpr t idx
      imul (($.)(-8)) rax # "compute offset amount"
      case tableLookup symTab s of
        Nothing -> error "unbound symbol passed typechecking!"
        Just (_, Nothing, _) -> lea s r12 # "load address of global array"
        Just (_, Just i, _)
          | i < 0 -> lea (i % rbp) r12 # "load address of local array"
          | i >= 0 -> mov (i % rbp) r12 # "load array argument address"
      add r12 rax # "compute address of array reference"
    _ -> error "attempt to address in a non-type-safe way"
  ArrayExp s idx symTab -> do
    genExpr t idx
    imul (($.)(-8)) rax # "compute offset amount"
    case tableLookup symTab s of
      Nothing -> error "unbound symbol passed typechecking!"
      Just (_, Nothing, _) -> lea s r12 # "load global array address"
      Just (_, Just i, _)
        | i < 0 -> lea (i % rbp) r12 # "load local array address"
        | i >= 0 -> mov (i % rbp) r12 # "load array argument address"
    add rax r12 # "compute actual offset"
    mov (0 r12) rax # "evaluate result"
  FuncExp fname args _ -> do
    forM_ (reverse args) $ \arg -> do
      genExpr t arg
      push rax # "push argument onto the stack"
    push rbp # "save frame pointer"
    call fname
    pop rbp # "restore frame pointer"
    add (($.) (8 * length args)) rsp # "pop arguments off the stack"
  ReadExp -> do
    mov (($.)0) rax # "clear return value"
    sub (($.)40) rsp # "decrement stack pointer for read()"
    lea (24 rsp) rsi # "pointer for read() result"
    mov readIntString rdi # "move format string into rdi"
    call scanf
    mov (24 rsp) rax # "put result in accumulator"
    add (($.)40) rsp # "pop stack"
  AssignExp v e' -> do
    genExpr t e'
    case v of
      IdVar s symTab -> case tableLookup symTab s of
        Nothing -> error "unbound symbol passed typechecking!"
        Just (_, Nothing, _) -> mov rax s # "assign to global variable"
        Just (_, Just i, _) -> mov rax (i % rbp) # "assign to local variable"
      ArrVar s idx symTab -> do
        push rax # "store result of expression"
        case tableLookup symTab s of
          Nothing -> error "unbound symbol passed typechecking!"
          Just (_, Nothing, _) -> lea s r12 # "load global array address"
          Just (_, Just i, _)
            | i < 0 -> lea (i % rbp) r12 # "load local array address"
            | i >= 0 -> mov (i % rbp) r12 # "load array argument address"
        genExpr t idx
        imul (($.)(-8)) rax # "compute offset amount"
        add rax r12 # "compute actual offset"
        pop rax # "get expression result back"
        mov rax (0 r12) # "assign to local variable"
      DerefVar s symTab -> case tableLookup symTab s of
        Nothing -> error "unbound symbol passed typechecking!"
        Just (_, Nothing, _) -> do
          mov s r12 # "get value of pointer"
          mov rax (0 r12) # "assign result to value of pointer"
        Just (_, Just i, _) -> do
          mov rbp r12 # "get base pointer"
          add (($.)i) r12 # "compute value of pointer"
          mov (0 r12) r12 # "dereference pointer"
          mov rax (0 r12) # "assign result to value of pointer"

genStmt :: M.Map String String -> String -> Statement SymbolTable -> CodeGen ()
genStmt t fname stmt = case stmt of
  CompoundStmt _ stmts -> mapM_ (genStmt t fname) stmts
  ExpressionStmt e -> genExpr t e
  IfStmt e s -> do
    l <- newLabel
    genExpr t e
    cmp (($.)0) rax # "compare result to 0"
    je l
    genStmt t fname s
    label l
  IfElseStmt e s1 s2 -> do
    els <- newLabel
    fin <- newLabel
    genExpr t e
    cmp (($.)0) rax # "compare result to 0"
    je els # "jump to else case"
    genStmt t fname s1
    jmp fin # "jump to end of if/else"
    label els # "else"
    genStmt t fname s2
    label fin # "end if/else"
  WhileStmt e s -> do
    end <- newLabel
    start <- newLabel
    label start # "top of loop"
    genExpr t e
    cmp (($.)0) rax # "compare result to 0"
    je end # "jump over statement"
    genStmt t fname s
    jmp start # "loop"
    label end # "end of loop"
  ReturnStmt e -> do
    case e of
      Nothing -> return ()
      Just e' -> genExpr t e'
    jmp $ "." ++ fname ++ "_ret"
  WriteStmt e -> do
    genExpr t e
    -- TODO: this is wrong; array references to string arrays will fail (wrong format string)
    writeStmt $ case e of
      StringExp _ -> writeStringString
      ArrayExp s _ symTab -> case tableLookup symTab s of
        Nothing -> "unbound symbol passed typechecking!"
        Just (_, _, typ) -> formatString typ
      _ -> writeIntString
    where formatString TString = writeStringString
          formatString (TStringArray _) = writeStringString
          formatString TStringPointer = writeStringString
          formatString _ = writeIntString
  WriteLnStmt -> writeStmt writeLnString

genDecl :: M.Map String String -> Declaration SymbolTable -> CodeGen ()
genDecl t (FDecl (FunDec _ fname _ stmt)) = do
  let varLength = localVarLength 0 stmt
  fname -: do
    mov rsp rbp # "move stack pointer to frame pointer"
    sub (($.)varLength) rsp # "allocate local vars"
    genStmt t fname stmt
  "." ++ fname ++ "_ret" -: do
    add (($.)varLength) rsp # "deallocate local vars"
    when (fname == "main") $ mov (($.)0) rax # "main should return 0"
    ret
  write "\n"
genDecl _ (VDecl (VarDec _ i name)) = write $ ".comm " ++ name ++ ", " ++ show i ++ ", 64"

allocateStrings :: M.Map String String -> CodeGen ()
allocateStrings t = forM_ (M.toAscList t) $ \(s, l) -> write $ l ++ ": .string " ++ show s

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
