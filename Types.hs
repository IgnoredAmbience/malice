module Types where
import Data.Map (Map)

data Token = TokDot AlexPosn
           | TokComma AlexPosn
           | TokQuestion AlexPosn
           | TokAnd AlexPosn
           | TokBut AlexPosn
           | TokThen AlexPosn
           | TokToo AlexPosn

           | TokAlice AlexPosn
           | TokFound AlexPosn
           | TokWas AlexPosn
           | TokA AlexPosn
           | TokBecame AlexPosn
           | TokAte AlexPosn
           | TokDrank AlexPosn
           | TokWhat AlexPosn
           | TokThought AlexPosn
           | TokSaid AlexPosn
           | TokSpoke AlexPosn

           | TokHad AlexPosn
           | TokArrS AlexPosn
           | TokPiece AlexPosn

           | TokThe AlexPosn
           | TokRoom AlexPosn
           | TokContained AlexPosn
           | TokLookingGlass AlexPosn
           | TokChanged AlexPosn
           | TokWent AlexPosn
           | TokThrough AlexPosn

           | TokEventually AlexPosn
           | TokBecause AlexPosn
           | TokEnoughTimes AlexPosn

           | TokPerhaps AlexPosn
           | TokEither AlexPosn
           | TokSo AlexPosn
           | TokOr AlexPosn
           | TokMaybe AlexPosn
           | TokUnsure AlexPosn
           | TokWhich AlexPosn

           | TokNumberType AlexPosn
           | TokLetterType AlexPosn
           | TokSentenceType AlexPosn

           | TokOp AlexPosn Char

           | TokLBrace AlexPosn
           | TokRBrace AlexPosn

           | TokInt AlexPosn Int
           | TokId AlexPosn String
           | TokChar AlexPosn Char
           | TokStr AlexPosn String
  deriving (Eq,Show)

data AlexPosn = AlexPn !Int !Int !Int -- AbsChar, Line, Char
  deriving (Eq,Show)

alexPosnLine (AlexPn _ l _) = l
alexPosnChar (AlexPn _ _ c) = c

data Program = Program [Statement] [Function]
  deriving (Eq,Show)
data Function = Function String Type [(Type, String)] [Statement]
              | Lambda String Type [Statement]
  deriving (Eq,Show)
data Statement = Declare String Type
               | DeclareArr String Type Exp     -- Name, Type, Length
               | Assign Variable Exp
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
data Type = Number | Letter | Sentence
  deriving (Eq,Show)
data Exp = UnOp UnOp Exp
         | BinOp BinOp Exp Exp
         | FunctionCall String [Exp]
         | Variable Variable
         | Int Int
         | Char Char
         | Str String
  deriving (Eq,Show)
data Variable = Var String
              | VarArr String Exp
  deriving (Eq,Show)
data BinOp = Or | Xor | And | Add | Sub | Mul | Div | Mod | LOr | LAnd | Eq | Neq | Lt | Lte | Gt | Gte
  deriving (Eq,Show)
data UnOp = Not | Neg
  deriving (Eq,Show)

type SymbolTbl = Map String Type

data SInst = SOr | SXor | SAnd | SAdd | SSub | SMul | SDiv | SMod | SNot | SInc | SDec
		  | SPushI Int | SPushN String | SPop String
  deriving (Eq,Show)
