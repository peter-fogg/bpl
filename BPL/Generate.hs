module BPL.Generate
       where

import BPL.Check (convertVarDec)
import BPL.DSL
import BPL.Types

convertExpr :: a -> Expr b -> Expr a
convertExpr x expr = case expr of
  CompExp l op r -> CompExp (convertExpr x l) op (convertExpr x r)
  ArithExp l op r -> ArithExp (convertExpr x l) op (convertExpr x r)
  IntExp i -> IntExp i
  StringExp s -> StringExp s
  DerefExp e y -> DerefExp (convertExpr x e) x
  AddrExp e y -> AddrExp (convertExpr x e) x
  ArrayExp s e y -> ArrayExp s (convertExpr x e) x
  FuncExp s es y -> FuncExp s (map (convertExpr x) es) x
  ReadExp -> ReadExp
  AssignExp v e -> AssignExp (convertVar x v) (convertExpr x e)
  where convertVar x' v = case v of
          IdVar s y -> IdVar s x
          ArrVar s e y -> ArrVar s (convertExpr x e) x
          DerefVar s y -> DerefVar s x

computeOffsets :: [Declaration SymbolTable] -> [Declaration (Maybe Int)]
computeOffsets decls = map computeOffsets' decls

computeOffsets' :: Declaration SymbolTable -> Declaration (Maybe Int)
computeOffsets' (VDecl v) = VDecl $ convertVarDec Nothing v
computeOffsets' (FDecl (FunDec t s decls stmt)) = FDecl $ FunDec t s decls' stmt'
  where decls' = fst $ foldl go ([], 1) decls
        go (xs, n) v = (xs ++ [convertVarDec (Just $ 8*n) v], succ n)
        stmt' = computeOffsetsStmt (-1) stmt

computeOffsetsStmt :: Int -> Statement SymbolTable -> Statement (Maybe Int)
computeOffsetsStmt n (CompoundStmt decls stmts) = CompoundStmt decls' stmts'
  where (decls', n') = foldl go ([], n) decls
        go (xs, i) v@(VarDec (TStringArray len) _ _) = (xs ++ [convertVarDec (Just $ 8*len*i) v], i - len)
        go (xs, i) v@(VarDec (TIntArray len) _ _) = (xs ++ [convertVarDec (Just $ 8*len*i) v], i - len)
        go (xs, i) v = (xs ++ [convertVarDec (Just $ 8*i) v], pred i)
        stmts' = map (computeOffsetsStmt n') stmts
computeOffsetsStmt n (ExpressionStmt e) = ExpressionStmt $ convertExpr Nothing e
computeOffsetsStmt n (IfStmt e s) = IfStmt (convertExpr Nothing e) (computeOffsetsStmt n s)
computeOffsetsStmt n (IfElseStmt e s1 s2) = IfElseStmt
                                            (convertExpr Nothing e)
                                            (computeOffsetsStmt n s1)
                                            (computeOffsetsStmt n s2)
computeOffsetsStmt n (WhileStmt e s) = WhileStmt (convertExpr Nothing e) (computeOffsetsStmt n s)
computeOffsetsStmt n (ReturnStmt e) = ReturnStmt $ fmap (convertExpr Nothing) e
computeOffsetsStmt n (WriteStmt e) = WriteStmt (convertExpr Nothing e)
computeOffsetsStmt n WriteLnStmt = WriteLnStmt

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
