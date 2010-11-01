module Types where

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
data Exp = Or Exp Exp
         | Xor Exp Exp
         | And Exp Exp
         | Add Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Not Exp
         | Int Int
         | Var String 
  deriving (Eq,Show)

