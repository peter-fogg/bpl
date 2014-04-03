module BPL.Check
       where

import Control.Arrow ((&&&))
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.List (foldl')
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
insertSymbolTable _ _ (ST []) = error "insertion into empty symbol table"

insertSymbolTable' :: String -> Declaration SymbolTable -> SymbolTable -> SymbolTable
insertSymbolTable' s decl symTab = insertSymbolTable s decl (extendSymbolTable symTab)

insertMultipleSymbolTable :: [(String, Declaration SymbolTable)] -> SymbolTable -> SymbolTable
insertMultipleSymbolTable [] symTab = symTab
insertMultipleSymbolTable l symTab = foldl' (\acc (k, v) -> insertSymbolTable k v acc) (extendSymbolTable symTab) l

createSymbolTable :: [Declaration ()] -> ([Declaration SymbolTable], SymbolTable)
createSymbolTable = foldl' go ([], ST [])
  where go (ds, symTab) decl = let (decl', symTab') = declSymTab symTab decl in (decl':ds, symTab')

insertVarDec :: VarDec -> SymbolTable -> SymbolTable
insertVarDec v@(VarDec _ s) = insertSymbolTable' s (VDecl v)

-- top level declarations
declSymTab :: SymbolTable -> Declaration () -> (Declaration SymbolTable, SymbolTable)
declSymTab symTab (VDecl v) = (VDecl v, insertVarDec v symTab)
declSymTab symTab (FDecl (FunDec typ s decls stmt)) = let
  symTab' = insertSymbolTable' s f' symTab
  declEntries = map (getName &&& VDecl) decls
  symTab'' = insertMultipleSymbolTable declEntries symTab'
  f' = FDecl $ FunDec typ s decls (stmtSymTab symTab'' stmt) in
  (f', symTab')

getName :: VarDec -> String
getName (VarDec _ s) = s

getType :: VarDec -> TypeSpecifier
getType (VarDec t _) = t

exprSymTab :: SymbolTable -> Expr () -> Expr SymbolTable
exprSymTab symTab expr = case expr of
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
  where newTable = foldl' (flip insertVarDec) symTab decls
stmtSymTab symTab (ExpressionStmt e) = ExpressionStmt (exprSymTab symTab e)
stmtSymTab symTab (IfStmt e s) = IfStmt (exprSymTab symTab e) (stmtSymTab symTab s)
stmtSymTab symTab (IfElseStmt e s1 s2) = IfElseStmt (exprSymTab symTab e) (stmtSymTab symTab s1) (stmtSymTab symTab s2)
stmtSymTab symTab (WhileStmt e s) = WhileStmt (exprSymTab symTab e) (stmtSymTab symTab s)
stmtSymTab symTab (ReturnStmt e) = ReturnStmt $ fmap (exprSymTab symTab) e
stmtSymTab symTab (WriteStmt e) = WriteStmt (exprSymTab symTab e)
stmtSymTab _ WriteLnStmt = WriteLnStmt

checkBinaryExp :: Expr SymbolTable -> Expr SymbolTable -> MaybeT (Writer String) TypeSpecifier
checkBinaryExp l r = do
  ltype <- checkExpr l
  rtype <- checkExpr r
  if (ltype, rtype) == (TInt, TInt)
    then report "binary operation" TInt >> return TInt
    else typeMismatch (show ltype) (show rtype)

checkExpr :: Expr SymbolTable -> MaybeT (Writer String) TypeSpecifier
checkExpr (CompExp l _ r) = checkBinaryExp l r
checkExpr (ArithExp l _ r) = checkBinaryExp l r
checkExpr (IntExp i) = report i TInt >> return TInt
checkExpr (StringExp s) = report s TString >> return TString
checkExpr (VarExp name symTab) = case tableLookup symTab name of
  Nothing -> undeclared
  Just (FDecl _) -> typeMismatch "function" "variable reference"
  Just (VDecl (VarDec typ s)) -> report s typ >> return typ
checkExpr (DerefExp name symTab) = case tableLookup symTab name of
  Nothing -> undeclared
  Just (FDecl _) -> typeMismatch "pointer derefence" "function"
  Just (VDecl (VarDec typ s)) -> case typ of
    TIntPointer -> report ("*" ++ s) TInt >> return TInt
    TStringPointer -> report ("*" ++ s) TString >> return TString
    _ -> typeMismatch "pointer dereference" (show typ)
checkExpr (AddrExp name symTab) = case tableLookup symTab name of
  Nothing -> undeclared
  Just (FDecl _) -> typeMismatch "address" "function"
  Just (VDecl (VarDec typ s)) -> case typ of
    TInt -> report ("&" ++ s) TIntPointer >> return TIntPointer
    TString -> report ("&" ++ s) TStringPointer >> return TStringPointer
    _ -> typeMismatch "address reference" (show typ)
checkExpr (ArrayExp s expr symTab) = do
  etype <- checkExpr expr
  unless (etype == TInt) $ typeMismatch "TInt" (show etype)
  case tableLookup symTab s of
    Just (VDecl (VarDec typ _)) -> case typ of
      TIntArray _ -> report (s ++ "[]") TInt >> return TInt
      TStringArray _ -> report (s ++ "[]") TString >> return TString
      _ -> typeMismatch "array reference" (show typ)
    _ -> typeMismatch "array reference" "function"
checkExpr (FuncExp name exprs symTab) = do
  funDec <- case tableLookup symTab name of
    Nothing -> undeclared
    Just (FDecl f) -> return f
    _ -> typeMismatch "function" "variable reference"
  let (FunDec typ s decls _) = funDec
      declTypes = map getType decls
  exprTypes <- mapM checkExpr exprs
  if declTypes == exprTypes
    then report (s ++ "()") typ >> return typ
    else typeMismatch (show declTypes) (show exprTypes)
checkExpr ReadExp = return TInt
checkExpr (AssignExp var expr) = do
  vtype <- checkVar var
  etype <- checkExpr expr
  if vtype == etype
    then return vtype
    else typeMismatch (show vtype) (show etype)

checkVar :: Var SymbolTable -> MaybeT (Writer String) TypeSpecifier
checkVar (IdVar name symTab) = case tableLookup symTab name of
  Nothing -> undeclared
  Just (FDecl _) -> typeMismatch "variable assigment" "function"
  Just (VDecl (VarDec typ _)) -> report name typ >> return typ
checkVar (ArrVar name expr symTab) = case tableLookup symTab name of
  Nothing -> undeclared
  Just (FDecl _) -> typeMismatch "array assignment" "function"
  Just (VDecl (VarDec typ _)) -> case typ of
    TIntArray _ -> checkArray TInt name
    TStringArray _ -> checkArray TString name
    _ -> typeMismatch "array assignment" "non-array"
    where checkArray t s = do
            etype <- checkExpr expr
            if etype == TInt
              then report (s ++ "[]") t >> return t
              else typeMismatch "array index" (show etype)
checkVar (DerefVar name symTab) = case tableLookup symTab name of
  Nothing -> undeclared
  Just (FDecl _) -> typeMismatch "pointer dereference" "function"
  Just (VDecl (VarDec typ _)) -> case typ of
    TIntPointer -> report ("*" ++ name) TInt >> return TInt
    TStringPointer -> report ("*" ++ name) TString >> return TString
    _ -> typeMismatch "pointer dereference" (show typ)

checkStmt :: Statement SymbolTable -> MaybeT (Writer String) TypeSpecifier
checkStmt (CompoundStmt _ stmts) = do
  types <- mapM checkStmt stmts
  let types' = filter (/= TVoid) types
  case types' of
    [] -> return TVoid
    t:ts -> if all (==t) ts
            then return t
            else typeMismatch (show t) "in return values"
checkStmt (ExpressionStmt expr) = checkExpr expr >> return TVoid
checkStmt (IfStmt expr stmt) = do
  etype <- checkExpr expr
  unless (etype == TInt) $ typeMismatch "TInt" (show etype)
  checkStmt stmt
checkStmt (IfElseStmt expr s1 s2) = do
  etype <- checkExpr expr
  unless (etype == TInt) $ typeMismatch "TInt" (show etype)
  t1 <- checkStmt s1
  t2 <- checkStmt s2
  case (t1, t2) of
    (TVoid, t2') -> return t2'
    (t1', TVoid) -> return t1'
    (t1', t2') -> if t1' == t2' then return t1' else typeMismatch (show t1') (show t2')
checkStmt (WhileStmt expr stmt) = do
  etype <- checkExpr expr
  unless (etype == TInt) $ typeMismatch "TInt" (show etype)
  checkStmt stmt
checkStmt (ReturnStmt expr) = maybe (return TVoid) checkExpr expr
checkStmt (WriteStmt expr) = checkExpr expr >> return TVoid
checkStmt WriteLnStmt = return TVoid

checkDecl :: Declaration SymbolTable -> MaybeT (Writer String) TypeSpecifier
checkDecl (VDecl _) = return TVoid
checkDecl (FDecl (FunDec t _ _ stmt)) = do
  stype <- checkStmt stmt
  unless (stype == t) $ typeMismatch (show t) (show stype)
  return TVoid

report :: (Show a) => a -> TypeSpecifier -> MaybeT (Writer String) ()
report e typ = lift $ tell $ "assigning " ++ show e ++ " type " ++ show typ ++ "\n"

typeMismatch :: String -> String -> MaybeT (Writer String) a
typeMismatch t1 t2 = tell ("type mismatch: " ++  t1 ++ ", " ++ t2) >> MaybeT (return Nothing)

undeclared :: MaybeT (Writer String) a
undeclared = tell "undeclared variable" >> MaybeT (return Nothing)