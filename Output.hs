module Output where
import Types

outputASM :: [SInst] -> [String]
outputASM = concatMap toASM ++ ["pop ebx"] ++ ["mov eax,1"] ++ ["int 0x80"]

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

toASM (SPushI i) = ["push " ++ (show i)]
toASM (SPushN n) = ["push [" ++ n ++ "]"]
toASM (SPop n) = ["pop [" ++ n ++ "]"]
