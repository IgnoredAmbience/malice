module Output (output) where
import Types
import Data.Map (toList, elems, mapWithKey)
-- import Data.List (init) --used for chopping random colon off global main. Hack, but quick fix

output :: DataTbl -> [SymbolTbl] -> [[MInst]] -> [[String]]
output dt st fns = [outputDataTable dt] ++ zipWith (++) (map outputSymbolTable st) (map outputASM fns)

outputASM :: [MInst] -> [String]
outputASM is@(i:_) = ["section .text"] ++ ["extern output_int"] ++ ["extern output_str"] ++ ["extern input_int"] ++ ["global "++ ((init.show) i)] ++ map show (is ++ [BinMOp MXor (Reg EAX) (Reg EAX), NonMOp MRet])
outputASM [] = []

outputSymbolTable :: SymbolTbl -> [String]
outputSymbolTable st = "section .bss" : elems (mapWithKey symbolToDef st)

symbolToDef :: String -> Type -> String
symbolToDef name Number = name ++ ":\tresd\t1"
symbolToDef name Letter = name ++ ":\tresb\t1"
symbolToDef _ (FunctionType _ _) = ""
symbolToDef _ (LambdaType _) = ""
symbolToDef name x = name ++ " TODO UNKNOWN " ++ show x

symbolToDef name x = name ++ " TODO UNKNOWN " ++ show x

outputDataTable :: DataTbl -> [String]
outputDataTable dt = ["section .data"] ++ concat ( zipWith (++) (map stringDef $ toList dt) (map stringLenDef $ elems dt) )

stringDef (value, hash) = [hash ++ ":\tdb\t`" ++ value ++ "`"]
stringLenDef hash = ["len" ++ hash ++ ":\tequ\t$-" ++ hash]

