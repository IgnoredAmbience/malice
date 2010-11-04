module Semantics where
import Types
import qualified Data.Map as Map

semantics :: Program -> SymbolTbl
semantics (Program stmts exp)
  = semanticExp st exp
    where
      st = foldl semanticSt Map.empty stmts

semanticSt :: SymbolTbl -> Statement -> SymbolTbl 
semanticSt st (Declare x t)
  | Map.member x st = error $ "Semantic error - " ++ show x ++ " is already defined"
  | otherwise       = Map.insert x t st
semanticSt st (Assign _ exp) = semanticExp st exp
semanticSt st _              = st

semanticExp :: SymbolTbl -> Exp -> SymbolTbl
semanticExp st (Var x)
  | Map.notMember x st = error $ "Semantic error - use of undefined variable  " ++ show x
  | otherwise          = st

semanticExp st (UnOp _ exp)    = semanticExp st exp
semanticExp st (BinOp _ e1 e2) = semanticExp st1 e2
  where
    st1 = semanticExp st e1
semanticExp st _ = st

