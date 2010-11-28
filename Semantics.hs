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
      funcTbls = map (buildFunctionSymbolTbl funcTbl) funcs

-- Add function definitions into (global) symbol table
addFunction :: SymbolTbl -> Function -> SymbolTbl
addFunction st (Function name typ ps _)
  -- Will attempt insertion, calling alreadyDefinedError if a conflict arises
  = Map.insertWithKey alreadyDefinedError name (FunctionType typ paramTypes) st
    where
      paramTypes = map snd ps

addFunction st (Lambda name typ _)
  = Map.insertWithKey alreadyDefinedError name (LambdaType typ) st

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
stmtsSmbTbl = foldl addStatement

-- Check statement against symbol table, building it as required
addStatement :: SymbolTbl -> Statement -> SymbolTbl 
addStatement st (Declare x t)
  = Map.insertWithKey alreadyDefinedError x t st

addStatement st (DeclareArr name typ size)
  = Map.insertWithKey alreadyDefinedError name (Array typ) st

addStatement st (Assign var exp) 
  | Map.notMember (name var) st = error $ "Semantic error - attempt to assign to undeclared variable " ++ show var
  | eType /= varType =
      error $ "Semantic error - declared type of " ++ show varType ++ " " ++ show var ++ " does not match inferred type " ++ show eType ++ " of expression"
  | otherwise = st
  where
    eType = expType st exp
    varType = st Map.! (name var)

addStatement st (Call exp) = checkExp st exp
addStatement st (Increment var) = undefined
addStatement st (Decrement var) = undefined
addStatement st (LambdaApply name var) = undefined
addStatement st (Input var) = undefined
addStatement st (Output exp)
  | isPrintable $ expType st exp = st
  | otherwise                    = semError "not printable"
addStatement st (Return exp) = undefined
addStatement st (LoopUntil exp sts) = undefined
addStatement st (If exp thenSts elseSts) = undefined
addStatement st (Comment _) = st

isPrintable :: Type -> Bool
isPrintable Number = True
isPrintable Letter = True
isPrintable Sentence = True
isPrintable _ = False

expType :: SymbolTbl -> Exp -> Type
expType st = fst . semanticExp st

checkExp :: SymbolTbl -> Exp -> SymbolTbl
checkExp st = snd . semanticExp st

semanticExp :: SymbolTbl -> Exp -> (Type, SymbolTbl)
semanticExp st (Int _)  = (Number, st)
semanticExp st (Char _) = (Letter, st)
semanticExp st (Str _)  = (Sentence, st)
semanticExp st (Variable (Var x)) = (Map.findWithDefault (undefinedError x) x st, st)
semanticExp st (Variable (VarArr x _)) = (t, st)
  where Array t = Map.findWithDefault (undefinedError x) x st

semanticExp st (UnOp _ exp)    = semanticExp st exp
semanticExp st (BinOp op e1 e2)
  | e1Type /= e2Type = error $ "Semantic error - Type mismatch on " ++ show op ++ " operator, " ++ show e1Type ++ " does not match " ++ show e2Type
  | otherwise        = (e1Type, st)
  where
    e1Type = expType st e1
    e2Type = expType st e2

-- Error messages
semError = error . (++) "Semantic error: " 
alreadyDefinedError k old new = semError $ "cannot redefine " ++ show k ++ " as a " ++ show new ++ ", already defined as a " ++ show old ++ "."
undefinedError x = semError $ "use of undefined variable " ++ show x

