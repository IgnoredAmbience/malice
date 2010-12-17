module StatementOptimise(stateOptimise) where
import Types
import Data.Maybe (fromMaybe)

import qualified Data.Map as M

stateOptimise :: [SymbolTbl] -> Program -> Program
stateOptimise symTab funcs = zipWith optimiseFunc symTab funcs
  where
    optimiseFunc :: SymbolTbl -> Function -> Function
    optimiseFunc st (Function n t a statements) = (Function n t a (removeVariables st statements))
    optimiseFunc st (Lambda n t statements) = (Lambda n t (removeVariables st statements))

removeVariables :: SymbolTbl -> [Statement] -> [Statement]
removeVariables symTab instrs = map mapExp instrs
  where
    singles = getSingleAssignVars symTab instrs
    values  = getVarValues singles instrs

    mapExp :: Statement -> Statement
    mapExp (Assign v e) = Assign v (mapVar e)
    mapExp (Output e)   = Output (mapVar e)
    mapExp (Return e)   = Return (mapVar e)
    mapExp (If e s s')  = If (mapVar e) (map mapExp s) (map mapExp s')
    mapExp x            = x

    mapVar :: Exp -> Exp
    mapVar ins@(Variable (Var x)) = fromMaybe ins $ M.lookup x values
    mapVar x       = x


getSingleAssignVars :: SymbolTbl -> [Statement] -> SymbolTbl
getSingleAssignVars symTab instrs = M.intersection symTab (M.map (const undefined) singleUseVars) 
--intersect takes the left operands val (as in <key,val>) values, but the values of both must be the same time, hence undefined
    where
      zeroedVars    = M.map (const 0) symTab
      varCounts     = foldr countVar zeroedVars instrs
      singleUseVars = M.filter ( == 1) varCounts

getVarValues :: SymbolTbl -> [Statement] -> M.Map String Exp
getVarValues symTab states = M.fromList $ map (\(Assign (Var v) e) -> (v,e)) $ appropriateVariables
  where
    appropriateVariables = filter isAppropriate states

    isAppropriate (Assign (Var x) _ ) =  M.member x symTab
    isAppropriate _ = False
    

countVar :: Statement -> (M.Map String Integer) -> (M.Map String Integer)
countVar (Increment (Var x))    = M.update (fmap succ.Just) x
countVar (Decrement (Var x))    = M.update (fmap succ.Just) x
countVar (LambdaApply _ (Var x))= M.update (fmap succ.Just) x
countVar (Input (Var x))        = M.update (fmap succ.Just) x
countVar (Assign (Var x) _ )    = M.update (fmap succ.Just) x
countVar _                      = id