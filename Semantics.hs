module Semantics where
import Types
import qualified Data.Map as Map

semantics :: Program -> (SymbolTbl, Type)
semantics (Program stmts exp)
  = (st, semanticExp st exp)
    where
      st = foldl semanticSt Map.empty stmts

-- Build up symbol table from statements, performing checks on any expressions as required
semanticSt :: SymbolTbl -> Statement -> SymbolTbl 
semanticSt st (Declare x t)
  | Map.member x st = error $ "Semantic error - " ++ show x ++ " is already defined"
  | otherwise       = Map.insert x t st

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

