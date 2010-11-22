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

           | TokEventually
           | TokBecause
           | TokEnough
           | TokTimes

           | TokPerhaps
           | TokSo
           | TokOr
           | TokMaybe
           | TokUnsure
           | TokWhich

           | TokNumberType
           | TokLetterType
           | TokSentenceType

           | TokUnOp Char
           | TokBinOp Char

           | TokLBrace
           | TokRBrace

           | TokInt Int
           | TokId String
           | TokChar Char
           | TokStr String
  deriving (Eq,Show)

data Program = Program [Statement] Exp 
  deriving (Eq,Show)
data Statement = Declare String Type
               | Assign String Exp
               | Increment String 
               | Decrement String 
  deriving (Eq,Show)
data Type = Number | Letter
  deriving (Eq,Show)
data Exp = UnOp UnOp Exp
         | BinOp BinOp Exp Exp
         | Int Int
         | Var String 
         | Char Char
  deriving (Eq,Show)
data BinOp = Or | Xor | And | Add | Sub | Mul | Div | Mod
  deriving (Eq,Show)
data UnOp = Not
  deriving (Eq,Show)

type SymbolTbl = Map String Type

data SInst = SOr | SXor | SAnd | SAdd | SSub | SMul | SDiv | SMod | SNot | SInc | SDec
		  | SPushI Int | SPushN String | SPop String
  deriving (Eq,Show)
