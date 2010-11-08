module Translator where
import Types

-- Translates statements/expressions/etc into a list of abstract Instructions
translate :: Program -> [SInst]
translate (Program statements return) = concatMap transStat statements ++ (transExp return)

transStat :: Statement -> [SInst]
transStat (Declare _ _) = []
transStat (Assign name exp) = (transExp exp) ++ [SPop name]
transStat (Increment name) = [SPushN name] ++ [SInc] ++ [SPop name]
transStat (Decrement name) = [SPushN name] ++ [SDec] ++ [SPop name]

transExp :: Exp -> [SInst]
transExp (Int i) = [SPushI i]
transExp (Var name) = [SPushN name]
transExp (UnOp Not exp) = (transExp exp) ++ [SNot]
transExp (BinOp op exp1 exp2) = (transExp exp1) ++ (transExp exp2) ++ (opToSOp op)

opToSOp :: BinOp -> [SInst]
opToSOp Or  = [SOr]
opToSOp Xor = [SXor]
opToSOp And = [SAnd]
opToSOp Add = [SAdd]
opToSOp Sub = [SSub]
opToSOp Mul = [SMul]
opToSOp Div = [SDiv]
opToSOp Mod = [SMod]
