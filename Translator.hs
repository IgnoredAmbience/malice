module Translator where
import Types

import Data.IORef
import System.IO.Unsafe

-- Please don't kill me...
l = unsafePerformIO $ newIORef 1

labelGet = do
	ll <- readIORef l
	writeIORef l $ ll + 1
	return ll

-- Translates statements/expressions/etc into a list of abstract Instructions
translate :: Program -> [SFn]
translate (Program functions) = map transFunc functions

transFunc :: Function -> [SInst]
transFunc (Function name t args stats) =
	[SLabel name] ++ (popArgs args) ++ (concatMap transStat stats)
	where
		popArgs []                  = []
		popArgs ((name,Number):as)  = (popArgs as) ++ [SPushN name]
		--popArgs ((name,Array _):as) = (popArgs as) ++ -- TODO: check pointer passing

transStat :: (Statement,Int) -> ([SInst],Int)
transStat (Declare _ _)                    = []

transStat (Assign (Var name) exp)          = (transExp exp) ++ [SPop name]
transStat (Assign (VarArr name index) exp) = (transExp exp) ++ (transExp index) ++ [SPop name]

transStat (Increment (Var name))           = [SPushN name] ++ [SInc] ++ [SPop name]
transStat (Increment (VarArr name index))  = (transExp index) ++ [SGet name] ++ [SInc] ++ (transExp index) ++ [SPut name]
transStat (Decrement (Var name))           = [SPushN name] ++ [SDec] ++ [SPop name]
transStat (Decrement (VarArr name index))  = (transExp index) ++ [SGet name] ++ [SDec] ++ (transExp index) ++ [SPut name]

transStat (Call (FunctionCall label args)) = (concatMap transExp args) ++ [SCall label]
transStat (Call _)                         = []

transStat (Return exp)                     = (transExp exp) ++ [SRet]

transStat (LambdaApply label arg)          = (transExp arg) ++ [SCall]

transStat (Input (Var name))               =
transStat (Input (VarArr name))            =

transStat (Output exp)                     =

transStat (LoopUntil cond body)            = [SLabel lbl] ++ (concatMap transStat body) ++ (transExp cond) ++ [SJTrue lbl]
	where lbl = "L"++(show labelGet)

transStat (If cond true false)             = (transExp cond) ++ [SJTrue lbl++"_true"] ++ [SJump lbl++"_false"] ++ [SLabel lbl++"_true"] ++ (concatMap transStat true) ++ [SJump lbl++"_end"] ++ [SLabel lbl++"_false"] ++ (concatMap transStat false) ++ [SLabel lbl++"_end"]
	where lbl = "L"++(show labelGet)

transStat (Comment _)                      = []

transExp :: Exp -> [SInst]
transExp (Int i) = [SPushI i]
transExp (Var name) = [SPushN name]
transExp (UnOp op exp) = (transExp exp) ++ (transUnOp op)
transExp (BinOp op exp1 exp2) = (transExp exp1) ++ (transExp exp2) ++ (transOp op)

transUnOp :: UnOp -> [SInst]
transUnOp Not = [SNot]
transUnOp Neg = [SNeg]

transOp :: BinOp -> [SInst]
transOp Or   = [SOr]
transOp Xor  = [SXor]
transOp And  = [SAnd]
transOp Add  = [SAdd]
transOp Sub  = [SSub]
transOp Mul  = [SMul]
transOp Div  = [SDiv]
transOp Mod  = [SMod]
transOp LOr  = [SLOr]
transOp LAnd = [SLand]
transOp Lt   = [SLt]
transOp Lte  = [SLte]
transOp Gt   = [SGt]
transOp Gte  = [SGte]
