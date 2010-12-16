module Output where
import Types
import Data.Map (mapWithKey, elems)

output :: [SymbolTbl] -> [SFn] -> [[String]]
output st fns = [x++y | (x,y) <- zip (map outputSymbolTable st) (map outputASM fns)]

outputASM :: [SInst] -> [String]
outputASM insts = ["section .text"] ++ ["global "++fname] ++ concatMap toASM insts ++ ["ret"]
	where (SLabel fname) = head insts

toASM :: SInst -> [String]
toASM SOr  = ["pop eax"] ++ ["or [esp],eax"]
toASM SXor = ["pop eax"] ++ ["xor [esp],eax"]
toASM SAnd = ["pop eax"] ++ ["and [esp],eax"]
toASM SAdd = ["pop eax"] ++ ["add [esp],eax"]
toASM SSub = ["pop eax"] ++ ["sub [esp],eax"]
toASM SMul = ["pop eax"] ++ ["imul eax,[esp]"] ++ ["mov [esp],eax"]
toASM SDiv = ["pop ebx"] ++ ["pop eax"] ++ ["xor edx,edx"] ++ ["idiv ebx"] ++ ["push eax"]
toASM SMod = ["pop ebx"] ++ ["pop eax"] ++ ["xor edx,edx"] ++ ["idiv ebx"] ++ ["push edx"]

-- TODO
-- IDEAS: Make labels by hashing a seed (eg, unix time at that point of compilation) with a salt, to help ensure the labels can't overlap
toASM SLOr = ["pop eax"] ++ ["pop ebx"] ++ ["cmp eax,ebx"] -- EAX || EBX
toASM SLAnd = ["pop eax"] ++ ["pop ebx"] -- EAX && EBX
toASM SEq = ["pop eax"] ++ ["pop ebx"] ++ ["cmp eax,ebx"] ++ ["je "{-LABEL-}] ++ ["mov eax,0"] ++ ["push eax"] ++ [{-LABEL-}] ++ ["mov eax,1"] ++ ["push eax"] -- EAX == EBX
toASM SNeq = ["pop eax"] ++ ["pop ebx"] ++ ["cmp eax,ebx"] ++ ["jne "{-LABEL-}] ++ ["mov eax,0"] ++ ["push eax"] ++ [{-LABEL-}] ++ ["mov eax,1"] ++ ["push eax"] -- EAX != EBX
toASM SLt = ["pop eax"] ++ ["pop ebx"] ++ ["cmp eax,ebx"] ++ ["jl "{-LABEL-}] ++ ["mov eax,0"] ++ ["push eax"] ++ [{-LABEL-}] ++ ["mov eax,1"] ++ ["push eax"] -- EAX < EBX
toASM SLte = ["pop eax"] ++ ["pop ebx"] ++ ["cmp eax,ebx"] ++ ["jle "{-LABEL-}] ++ ["mov eax,0"] ++ ["push eax"] ++ [{-LABEL-}] ++ ["mov eax,1"] ++ ["push eax"] -- EAX <= EBX
toASM SGt = ["pop eax"] ++ ["pop ebx"] ++ ["cmp eax,ebx"] ++ ["jg "{-LABEL-}] ++ ["mov eax,0"] ++ ["push eax"] ++ [{-LABEL-}] ++ ["mov eax,1"] ++ ["push eax"] -- EAX > EBX
toASM SGte = ["pop eax"] ++ ["pop ebx"] ++ ["cmp eax,ebx"] ++ ["jge "{-LABEL-}] ++ ["mov eax,0"] ++ ["push eax"] ++ [{-LABEL-}] ++ ["mov eax,1"] ++ ["push eax"] -- EAX >= EBX

toASM SNot = ["not dword [esp]"]
toASM SNeg = ["neg dword [esp]"]
toASM SInc = ["inc dword [esp]"]
toASM SDec = ["dec dword [esp]"]

-- TODO
toASM (SPrintS s) = []
toASM SPrintI = ["pop eax"]

toASM (SPushI i) = ["mov eax," ++ (show i)] ++ ["push eax"]
toASM (SPushN n) = ["mov eax,[" ++ n ++ "]"] ++ ["push eax"]
toASM (SPop n) = ["pop eax"] ++ ["mov [" ++ n ++ "],eax"]
-- TODO: Change these to index from 1 with bounds checking. Also, figure out how/when to get the bound into the array first...
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
symbolToDef _ (FunctionType _ _) = ""
symbolToDef _ (LambdaType _) = ""
--symbolToDef name Sentence = 
--symbolToDef name (Array a) =
symbolToDef name x = name ++ " TODO UNKNOWN " ++ show x
