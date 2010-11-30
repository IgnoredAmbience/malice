module Output where
import Types
import Data.Map (mapWithKey, elems)

output :: SymbolTbl -> [SFn] -> [[String]]
output st fns = outputSymbolTable st ++ map outputASM fns

outputASM :: [SInst] -> [String]
outputASM insts = ["section .text"] ++ ["global _start"] ++ ["_start:"] ++ concatMap toASM insts ++ ["pop ebx"] ++ ["mov eax,1"] ++ ["int 0x80"]

toASM :: SInst -> [String]
toASM SOr  = ["pop eax"] ++ ["or [esp],eax"]
toASM SXor = ["pop eax"] ++ ["xor [esp],eax"]
toASM SAnd = ["pop eax"] ++ ["and [esp],eax"]
toASM SAdd = ["pop eax"] ++ ["add [esp],eax"]
toASM SSub = ["pop eax"] ++ ["sub [esp],eax"]
toASM SMul = ["pop eax"] ++ ["imul eax,[esp]"] ++ ["mov [esp],eax"]
toASM SDiv = ["pop ebx"] ++ ["pop eax"] ++ ["xor edx,edx"] ++ ["idiv ebx"] ++ ["push eax"]
toASM SMod = ["pop ebx"] ++ ["pop eax"] ++ ["xor edx,edx"] ++ ["idiv ebx"] ++ ["push edx"]
{-
toASM SLor
toASM SLAnd
toASM SEq
toASM SNeq
toASM SLt
toASM SLte
toASM SGt
toASM SGte
-}

toASM SNot = ["not dword [esp]"]
toASM SNeg = ["neg dword [esp]"]
toASM SInc = ["inc dword [esp]"]
toASM SDec = ["dec dword [esp]"]

toASM (SPushI i) = ["mov eax," ++ (show i)] ++ ["push eax"]
toASM (SPushN n) = ["mov eax,[" ++ n ++ "]"] ++ ["push eax"]
toASM (SPop n) = ["pop eax"] ++ ["mov [" ++ n ++ "],eax"]
toASM (SGet n) = ["pop eax"] ++ ["dec eax"] ++ ["mov ebx,"++n] ++ ["mov eax,[ebx + 4*eax]"] ++ ["push eax"]
toASM (SPut n) = ["pop eax"] ++ ["dec eax"] ++ ["mov ebx,"++n] ++ ["pop ecx"] ++ ["mov [ebx + 4*eax],ecx"]

toASM (SLabel label) = [label++":"]
toASM (SJump label)  = ["jmp "++label]
toASM (SJTrue label) = ["pop eax"] ++ ["cmp eax,0"] ++ ["jne "++label]
toASM (SCall label)  = ["call "++label]
toASM (SRet)         = ["ret"]

outputSymbolTable :: SymbolTbl -> [String]
outputSymbolTable st = "section .bss" : elems (mapWithKey symbolToDef st)

symbolToDef :: String -> Type -> String
symbolToDef name Number = name ++ ":\tresd\t1"
symbolToDef name Letter = name ++ ":\tresb\t1"
