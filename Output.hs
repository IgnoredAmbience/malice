module Output (output) where
import Types
import Data.Map (toList, elems, mapWithKey)

output :: DataTbl -> [SymbolTbl] -> [[MInst]] -> [[String]]
output dt st fns = [outputDataTable dt] ++ zipWith (++) (map outputSymbolTable st) (map outputASM fns)

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
--symbolToDef name Sentence = 
--symbolToDef name (Array a) =
symbolToDef name x = name ++ " TODO UNKNOWN " ++ show x

outputDataTable :: DataTbl -> [String]
outputDataTable dt = ["section .data"] ++ (map (\(value, hash) -> hash ++ ":\tdb\t`" ++ value ++ "`") $ toList dt)

