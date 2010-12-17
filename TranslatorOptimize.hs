module TranslatorOptimize(transOptimize) where
import Types
import Data.Bits


transOptimize :: [SymbolTbl] -> [[SInst]] -> [[SInst]]
transOptimize symTab = {-zipWith removeVariables symTab.-} map constantFold


constantFold :: [SInst] -> [SInst]
constantFold input
    | input == folded = folded
    | otherwise       = constantFold folded
        where folded = constantFold' input

constantFold' :: [SInst] -> [SInst]

constantFold' (SPushI i : SNot : ss) = (SPushI (complement i) : constantFold' ss)
constantFold' (SPushI i : SNeg : ss) = (SPushI (-i) : constantFold' ss)
constantFold' (SPushI i1 : SPushI i2 : SOr : ss) = (SPushI (i1.|.i2) : constantFold' ss)
constantFold' (SPushI i1 : SPushI i2 : SXor : ss) = (SPushI (xor i1 i2) : constantFold' ss)
constantFold' (SPushI i1 : SPushI i2 : SAnd : ss) = (SPushI (i1.&.i2) : constantFold' ss)
constantFold' (SPushI i1 : SPushI i2 : SAdd : ss) = (SPushI (i1+i2) : constantFold' ss)
constantFold' (SPushI i1 : SPushI i2 : SSub : ss) = (SPushI (i1-i2) : constantFold' ss)
constantFold' (SPushI i1 : SPushI i2 : SMul : ss) = (SPushI (i1*i2) : constantFold' ss)
constantFold' (SPushI i1 : SPushI i2 : SDiv : ss) = (SPushI (div i1 i2) : constantFold' ss)
constantFold' (SPushI i1 : SPushI i2 : SMod : ss) = (SPushI (mod i1 i2) : constantFold' ss)

constantFold' (s:ss) = (s:constantFold' ss)
constantFold' [] = []

removeVariables :: SymbolTbl -> [SInst] -> [SInst]
removeVariables = undefined


getVarCount :: SymbolTbl -> [SInst] -> [(String,Type,Integer)]
getVarCount symTab instrs = undefined -- toList symTab
    where
      x = undefined

