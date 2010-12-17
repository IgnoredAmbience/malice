module Types where
import Data.Map (Map)
import Data.HashTable (hashString)

data Token = TokDot
           | TokComma
           | TokQuestion
           | TokAnd
           | TokBut
           | TokThen
           | TokToo

           | TokAlice
           | TokFound
           | TokWas
           | TokA
           | TokBecame
           | TokAte
           | TokDrank
           | TokWhat
           | TokThought
           | TokSaid
           | TokSpoke

           | TokHad
           | TokArrS
           | TokPiece

           | TokThe
           | TokRoom
           | TokContained
           | TokSpider
           | TokLookingGlass
           | TokChanged
           | TokWent
           | TokThrough

           | TokEventually
           | TokBecause
           | TokEnoughTimes

           | TokPerhaps
           | TokEither
           | TokSo
           | TokOr
           | TokMaybe
           | TokUnsure
           | TokWhich

           | TokNumberType
           | TokLetterType
           | TokSentenceType

           | TokOp Char

           | TokLBrace
           | TokRBrace

           | TokInt Int
           | TokId String
           | TokChar Char
           | TokStr String
  deriving (Eq,Show)

data AlexPosn = AlexPn { index :: !Int
                       , line :: !Int
                       , lineIdx :: !Int
                       }
  deriving (Eq,Show)

type Program = [Function]
    
data Function = Function String Type [(String, Type)] [Statement]
              | Lambda String Type [Statement]
  deriving (Eq,Show)
           
data Statement = Declare String Type
               | DeclareArr String Type Exp     -- Name, Type, Length
               | Assign Variable Exp
               | Call Exp           -- Essentially null operation, except when a function called
               | Increment Variable 
               | Decrement Variable 
               | LambdaApply String Variable
               | Input Variable
               | Output Exp
               | Return Exp
               | LoopUntil Exp [Statement]
               | If Exp [Statement] [Statement]
               | Comment String
  deriving (Eq,Show)
           
data Type = Number | Letter | Sentence | Array Type | FunctionType Type [Type] | LambdaType Type
  deriving (Eq,Show)
           
data Exp = UnOp UnOp Exp
         | BinOp BinOp Exp Exp
         | FunctionCall String [Exp]
         | Variable Variable
         | Int Int
         | Char Char
         | Str String
  deriving (Eq,Show)
           
data Variable = Var {name :: String} | VarArr {name :: String, exp :: Exp}
  deriving (Eq,Show)
           
data BinOp = Or | Xor | And | Add | Sub | Mul | Div | Mod | LOr | LAnd | Eq | Neq | Lt | Lte | Gt | Gte
  deriving (Eq,Show)
  
data UnOp = Not | Neg
  deriving (Eq,Show)

-- (Global vars, [Function vars])
type SymbolTbl = Map String Type
type DataTbl = Map String String

lblStr s = "str" ++ show(abs$hashString s)
lblStrLen h = "len" ++ h

type SFn = [SInst]

data SInst = SOr | SXor | SAnd | SAdd | SSub | SMul | SDiv | SMod | SEq | SNeq | SLt | SLte | SGt | SGte -- 2 operand instructions
           | SNot | SNeg | SInc | SDec -- 1 operand instructions
		   | SPushI Int | SPushN String | SPop String | SGet String | SPut String -- Data manipulation instructions
		   | SLabel String | SJump String | SJTrue String | SJFalse String | SCall String | SEnter | SRestEnter | SRet | SPushEax -- Compiler directives
		   | SJEq String | SJNeq String | SJLt String | SJLte String | SJGt String | SJGte String
		   | SPrintI | SPrintS String | SInput -- Output/Input
           | SShiftL Int | SShiftR Int -- For multiplication/division hacks
           | SMalloc String
  deriving (Eq)


----------------------------------------
data MInst = BinMOp BinMInst AsmOp AsmOp
           | UnMOp  UnMInst  AsmOp
           | JmpMOp JmpInst Lbl
           | Label Lbl
           | NonMOp NonMInst

data BinMInst = MOr | MXor | MAnd | MAdd | MSub | MMul | MLOr | MCmp | MMov | MShl | MShr-- 2 operand instructions
data UnMInst  = MDiv | MMod | MNot | MNeg | MInc | MDec | MPush | MPop  -- 1 operand instructions
data JmpInst  = MJmp | MJGE | MJG | MJLE | MJL | MJE | MJNE | MCall 
data NonMInst = MRet | MLeave | MEnter | MPushA | MPopA -- 0 operand instructions

data AsmOp = Reg Reg
           | Const Int
           | Name String
           | ConstName String
           | Indirect AsmOp
           | IndirectScale AsmOp Scale AsmOp
           | DWord AsmOp
  deriving (Eq)           

data Scale = One 
           | Two 
           | Four 
           | Eight 
  deriving (Eq)

data Reg = EAX
         | EBX
         | ECX
         | EDX
         | ESI
         | EDI
         | ESP
         | EBP
  deriving (Eq)

  
data Lbl = Lbl String 

instance Show MInst where
    show (BinMOp o a b) = show o ++ " " ++ show a ++ "," ++ show b --o is not the same type as a & b
    show (UnMOp o a)    = show o ++ " " ++ show a
    show (JmpMOp j l)   = show j ++ " " ++ show l
    show (NonMOp i )    = show i
    show (Label l  )    = show l ++ ":"


instance Show BinMInst where
    show MOr = "or"
    show MXor = "xor"
    show MAnd = "and"
    show MAdd = "add"
    show MSub = "sub"
    show MMul = "imul"
    show MLOr = "or"
    show MCmp = "cmp"
    show MMov = "mov"
    show MShl = "sal"
    show MShr = "sar"

instance Show UnMInst where
    show MNot = "not"
    show MNeg = "neg"
    show MInc = "inc"
    show MDec = "dec"
    show MPush = "push"
    show MPop = "pop"
    show MDiv = "idiv"
    show MMod = "idiv"

instance Show NonMInst where
    show MRet = "ret"
    show MLeave = "leave"
    show MEnter = "enter"
    show MPopA  = "popa"
    show MPushA = "push"

instance Show JmpInst where
    show MJmp  = "jmp"
    show MJGE  = "jge"
    show MJG   = "jg"
    show MJLE  = "jle"
    show MJL   = "jl"
    show MJE   = "je"
    show MJNE  = "jne"
    show MCall = "call"

instance Show AsmOp where
    show (Reg r)       = show r
    show (Const i)     = show i
    show (Name  s)     = concat ["[",s,"]"]
    show (ConstName s) = s
    show (Indirect a)  = concat ["[", show a, "]"]
    show (IndirectScale a s b) = concat ["[", show a, "+", show s, "*", show b, "]"]
    show (DWord a)     = "dword " ++ show a

instance Show Lbl where
    show (Lbl s) = s

instance Show Scale where
    show One = "1"
    show Two = "2"
    show Four = "4"
    show Eight = "8"

instance Show Reg where
    show EAX = "eax"
    show EBX = "ebx"
    show ECX = "ecx"
    show EDX = "edx"
    show ESI = "esi"
    show EDI = "edi"
    show ESP = "esp"
    show EBP = "ebp"
