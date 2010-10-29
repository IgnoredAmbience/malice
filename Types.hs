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

data Program = Program [Statement] Output
  deriving (Eq,Show)
data Output = Result Exp
  deriving (Eq,Show)
data Statement = Declare String Type
               | Assign String Exp
               | Increment String 
               | Decrement String 
  deriving (Eq,Show)
data Type = Number
  deriving (Eq,Show)
data Exp = Or Exp Exp1
         | Xor Exp Exp1
         | And Exp Exp1
         | Exp1 Exp1
  deriving (Eq,Show)
data Exp1 = Add Exp1 Exp2
          | Exp2 Exp2
  deriving (Eq,Show)
data Exp2 = Times Exp2 Exp3
          | Div Exp2 Exp3
          | Mod Exp2 Exp3
          | Exp3 Exp3
  deriving (Eq,Show)
data Exp3 = Not Exp3
          | Val Val
  deriving (Eq,Show)
data Val = Int Int
         | Var String 
  deriving (Eq,Show)

