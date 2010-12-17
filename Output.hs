module Output (output) where
import Types
import Data.Map (mapWithKey, elems)

output :: [SymbolTbl] -> [[MInst]] -> [[String]]
output st fns = zipWith (++) (map outputSymbolTable st) (map outputASM fns)

outputASM :: [MInst] -> [String]
outputASM is@(i:_) = ["section .text"] ++ ["global "++ show i] ++ map show is ++ ["ret"]
outputASM [] = []

outputSymbolTable :: SymbolTbl -> [String]
outputSymbolTable st = "section .bss" : elems (mapWithKey symbolToDef st)

symbolToDef :: String -> Type -> String
symbolToDef name Number = name ++ ":\tresd\t1"
symbolToDef name Letter = name ++ ":\tresb\t1"
symbolToDef _ (FunctionType _ _) = ""
symbolToDef _ (LambdaType _) = ""

symbolToDef name x = name ++ " TODO UNKNOWN " ++ show x

