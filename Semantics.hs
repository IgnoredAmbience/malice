module Semantics where
import Types
import qualified Data.Map as Map

-- Some of these repeated definitions could be probably cleaned up a bit

semantics :: Program -> ProgramSymbolTbl
semantics (Program stmts funcs)
  = (mainTbl, funcTbls)
    where
      funcTbl = foldl addFunction Map.empty funcs
      mainTbl = foldl addStatement funcTbl stmts
      funcTbls = map (foldl addStatement funcTbl) funcs

-- Add function definitions into (global) symbol table
addFunction :: SymbolTbl -> Function -> SymbolTbl
addFunction st (Function name typ ps _)
  | Map.member name st = error $ "Semantic error - " ++ show name ++ "is already defined"
  | otherwise          = Map.insert name (FunctionType typ paramTypes) st
  where
    paramTypes = map fst ps

addFunction st (Lambda name typ _)
  | Map.member name st = error $ "Semantic error - " ++ show name ++ "is already defined"
  | otherwise          = Map.insert name (LambdaType typ) st

-- Build symbol table for a function
buildFunctionSymbolTbl :: SymbolTbl -> Function -> SymbolTbl
buildFunctionSymbolTbl funcSt (Function _ _ params stmts)
  = foldl addStatement st stmts
    where
      st = Map.unionWithKey alreadyDefinedError funcSt (Map.fromList params) 
buildFunctionSymbolTbl funcSt (Lambda _ typ stmts)
  = foldl addStatement st stmts
    where
      st = Map.unionWithKey alreadyDefinedError funcSt (Map.singleton "it" typ) 

-- Build symbol table from a list of statements
stmtsSmbTbl :: SymbolTbl -> [Statement] -> SymbolTbl
stmtsSmbTbl = foldl semanticSt

-- Check statement against symbol table, building it as required
semanticSt :: SymbolTbl -> Statement -> SymbolTbl 
semanticSt st (Declare x t)
  | Map.member x st = error $ "Semantic error - " ++ show x ++ " is already defined"
  | otherwise       = Map.insert x t st

semanticSt st (DeclareArr name typ size)
  | Map.member name st = error $ "Semantic error - " ++ show name ++ " is already defined"
  | otherwise          = Map.insert name (Array typ) st

semanticSt st (Assign var exp) 
  | Map.notMember var st = error $ "Semantic error - attempt to assign to undeclared variable " ++ show var
  | expType /= varType =
      error $ "Semantic error - declared type of " ++ show varType ++ " " ++ show var ++ " does not match inferred type " ++ show expType ++ " of expression"
  | otherwise = st
  where
    expType = semanticExp st exp
    varType = st Map.! var

semanticSt st _              = st



semanticExp :: SymbolTbl -> Exp -> Type
semanticExp _ (Int _)  = Number
semanticExp _ (Char _) = Letter
semanticExp st (Var x)
  | Map.notMember x st = error $ "Semantic error - use of undefined variable  " ++ show x
  | otherwise          = st Map.! x

semanticExp st (UnOp _ exp)    = semanticExp st exp
semanticExp st (BinOp op e1 e2)
  | e1Type /= e2Type = error $ "Semantic error - Type mismatch on " ++ show op ++ " operator, " ++ show e1Type ++ " does not match " ++ show e2Type
  | otherwise        = e1Type
  where
    e1Type = semanticExp st e1
    e2Type = semanticExp st e2

-- Error messages
semError = error . (++) "Semantic error: " 
alreadyDefinedError k old new = semError $ "cannot redefine " ++ show k ++ " as a " ++ show new ++ ", already defined as a " ++ show old ++ "."

