module Types where
import Data.Map (Map)

data Token = TokDot
           | TokComma
           | TokAnd
           | TokBut
           | TokThen
           | TokAliceFound
           | TokWasA
           | TokBecame
           | TokAte
           | TokDrank
           | TokNumber
           | TokToo
           | TokUnOp Char
           | TokBinOp Char
           | TokInt Int
           | TokId String
  deriving (Eq,Show)

data Program = Program [Statement] Exp 
  deriving (Eq,Show)
data Statement = Declare String Type
               | Assign String Exp
               | Increment String 
               | Decrement String 
  deriving (Eq,Show)
data Type = Number
  deriving (Eq,Show)
data Exp = UnOp UnOp Exp
         | BinOp BinOp Exp Exp
         | Int Int
         | Var String 
  deriving (Eq,Show)
data BinOp = Or | Xor | And | Add | Times | Div | Mod
  deriving (Eq,Show)
data UnOp = Not
  deriving (Eq,Show)

type SymbolTbl = Map String Type

