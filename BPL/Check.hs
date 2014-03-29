module BPL.Check
       where

import Control.Arrow ((&&&))
import qualified Data.Map as M

import BPL.Types

tableLookup :: SymbolTable -> String -> Maybe (Declaration SymbolTable)
tableLookup (ST symTab) s = go symTab
  where go [] = Nothing
        go (t:ts) = case M.lookup s t of
          Nothing -> go ts
          result -> result

extendSymbolTable :: SymbolTable -> SymbolTable
extendSymbolTable (ST ts) = ST $ M.empty:ts

insertSymbolTable :: String -> Declaration SymbolTable -> SymbolTable -> SymbolTable
insertSymbolTable s decl (ST (t:ts)) = ST (M.insert s decl t:ts)

insertSymbolTable' :: String -> Declaration SymbolTable -> SymbolTable -> SymbolTable
insertSymbolTable' s decl symTab = insertSymbolTable s decl (extendSymbolTable symTab)

insertMultipleSymbolTable :: [(String, Declaration SymbolTable)] -> SymbolTable -> SymbolTable
insertMultipleSymbolTable l symTab = foldl (\acc (k, v) -> insertSymbolTable k v acc) (extendSymbolTable symTab) l

createSymbolTable :: [Declaration ()] -> [Declaration SymbolTable]
createSymbolTable decls = fst $ foldl go ([], ST []) decls
  where go (ds, symTab) decl = let (decl', symTab') = declSymTab symTab decl in (decl':ds, symTab')

insertVarDec :: VarDec -> SymbolTable -> SymbolTable
insertVarDec v@(VarDec _ s) symTab = insertSymbolTable' s (VDecl v) symTab
insertVarDec p@(PointerDec _ s) symTab = insertSymbolTable' s (VDecl p) symTab
insertVarDec a@(ArrayDec _ s _) symTab = insertSymbolTable' s (VDecl a) symTab

-- top level declarations
declSymTab :: SymbolTable -> Declaration () -> (Declaration SymbolTable, SymbolTable)
declSymTab symTab (VDecl v) = (VDecl v, insertVarDec v symTab)
declSymTab symTab (FDecl f@(FunDec typ s decls stmt)) = let
  newEntries = (s, f'):map (getName &&& VDecl) decls
  symTab' = insertMultipleSymbolTable newEntries symTab
  f' = FDecl $ FunDec typ s decls (stmtSymTab symTab' stmt) in
  (f', symTab')

getName :: VarDec -> String
getName (VarDec _ s) = s
getName (PointerDec _ s) = s
getName (ArrayDec _ s _) = s

exprSymTab :: SymbolTable -> Expr () -> Expr SymbolTable
exprSymTab symTab e = case e of
  CompExp r op l -> CompExp (exprSymTab symTab r) op (exprSymTab symTab l)
  ArithExp r op l -> ArithExp (exprSymTab symTab r) op (exprSymTab symTab l)
  IntExp i -> IntExp i
  StringExp s -> StringExp s
  VarExp s _ -> VarExp s symTab
  DerefExp s _ -> DerefExp s symTab
  AddrExp s _ -> AddrExp s symTab
  ArrayExp s e _ -> ArrayExp s (exprSymTab symTab e) symTab
  FuncExp s es _ -> FuncExp s (map (exprSymTab symTab) es) symTab
  ReadExp -> ReadExp
  AssignExp v e -> AssignExp (varSymTab symTab v) (exprSymTab symTab e)

varSymTab :: SymbolTable -> Var () -> Var SymbolTable
varSymTab symTab v = case v of
  IdVar s _ -> IdVar s symTab
  ArrVar s e _ -> ArrVar s (exprSymTab symTab e) symTab
  DerefVar s _ -> DerefVar s symTab

stmtSymTab :: SymbolTable -> Statement () -> Statement SymbolTable
stmtSymTab symTab (CompoundStmt decls stmts) = CompoundStmt decls (map (stmtSymTab newTable) stmts)
  where newTable = foldl (flip insertVarDec) symTab decls
stmtSymTab symTab (ExpressionStmt e) = ExpressionStmt (exprSymTab symTab e)
stmtSymTab symTab (IfStmt e s) = IfStmt (exprSymTab symTab e) (stmtSymTab symTab s)
stmtSymTab symTab (IfElseStmt e s1 s2) = IfElseStmt (exprSymTab symTab e) (stmtSymTab symTab s1) (stmtSymTab symTab s2)
stmtSymTab symTab (WhileStmt e s) = WhileStmt (exprSymTab symTab e) (stmtSymTab symTab s)
stmtSymTab symTab (ReturnStmt e) = ReturnStmt $ fmap (exprSymTab symTab) e
stmtSymTab symTab (WriteStmt e) = WriteStmt (exprSymTab symTab e)
stmtSymTab symTab WriteLnStmt = WriteLnStmt
