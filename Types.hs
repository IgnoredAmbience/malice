module Types where
import Data.Map (Map)

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
