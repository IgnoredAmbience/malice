module Output where
import Types
import Data.Map (mapWithKey, elems)

output :: SymbolTbl -> [SInst] -> [String]
output st insts = outputSymbolTable st ++ outputASM insts

outputASM :: [SInst] -> [String]
outputASM insts = ["section .text"] ++ ["global _start"] ++ ["_start:"] ++ concatMap toASM insts ++ ["pop ebx"] ++ ["mov eax,1"] ++ ["int 0x80"]

toASM :: SInst -> [String]

toASM SNot = ["not [esp]"]
toASM SOr  = ["pop eax"] ++ ["or [esp],eax"]
toASM SXor = ["pop eax"] ++ ["xor [esp],eax"]
toASM SAnd = ["pop eax"] ++ ["and [esp],eax"]
toASM SAdd = ["pop eax"] ++ ["add [esp],eax"]
toASM SSub = ["pop eax"] ++ ["sub [esp],eax"]
toASM SMul = ["pop eax"] ++ ["imul eax,[esp]"] ++ ["mov [esp],eax"]
toASM SDiv = ["pop ebx"] ++ ["pop eax"] ++ ["xor edx,edx"] ++ ["idiv ebx"] ++ ["push eax"]
toASM SMod = ["pop ebx"] ++ ["pop eax"] ++ ["xor edx,edx"] ++ ["idiv ebx"] ++ ["push edx"]
toASM SInc = ["inc [esp]"]
toASM SDec = ["dec [esp]"]

toASM (SPushI i) = ["mov eax," ++ (show i)] ++ ["push eax"]
toASM (SPushN n) = ["mov eax,[" ++ n ++ "]"] ++ ["push eax"]
toASM (SPop n) = ["pop eax"] ++ ["mov [" ++ n ++ "],eax"]

outputSymbolTable :: SymbolTbl -> [String]
outputSymbolTable st = "section .bss" : elems (mapWithKey symbolToDef st)

symbolToDef :: String -> Type -> String
symbolToDef name Number = name ++ ":\tresd\t1"
symbolToDef name Letter = name ++ ":\tresb\t1"

