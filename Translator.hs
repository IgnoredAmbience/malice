module Translator where
import Types

import Data.IORef
import System.IO.Unsafe

-- Translates statements/expressions/etc into a list of abstract Instructions
translate :: Program -> [SFn]
translate functions = map transFunc functions

transFunc :: Function -> [SInst]
transFunc (Function name t args stats) =
	[SLabel name] ++ (popArgs args) ++ code
	where
		(code,_) = transStat (stats,1)
		popArgs :: [(String,Type)] -> [SInst]
		popArgs []                  = []
		popArgs ((name,Number):as)  = (popArgs as) ++ [SPushN name]
		--popArgs ((name,Array _):as) = (popArgs as) ++ -- TODO: check pointer passing

transStat :: ([Statement],Int) -> ([SInst],Int)
transStat (((Declare _ _):ss),l)           = (out,l)
	where (out,_) = transStat (ss,l)

transStat (((Assign (Var name) exp):ss),l) = ((transExp exp) ++ [SPop name] ++ out,l)
	where (out,_) = transStat (ss,l)

transStat (((Assign (VarArr name index) exp):ss),l) = ((transExp exp) ++ (transExp index) ++ [SPut name] ++ out,l)
	where (out,_) = transStat (ss,l)

transStat (((Increment (Var name)):ss),l)   = ([SPushN name] ++ [SInc] ++ [SPop name] ++ out,l)
	where (out,_) = transStat (ss,l)
transStat (((Increment (VarArr name index)):ss),l)  = ((transExp index) ++ [SGet name] ++ [SInc] ++ (transExp index) ++ [SPut name] ++ out,l)
	where (out,_) = transStat (ss,l)
transStat (((Decrement (Var name)):ss),l)           = ([SPushN name] ++ [SDec] ++ [SPop name] ++ out,l)
	where (out,_) = transStat (ss,l)
transStat (((Decrement (VarArr name index)):ss),l)  = ((transExp index) ++ [SGet name] ++ [SDec] ++ (transExp index) ++ [SPut name] ++ out,l)
	where (out,_) = transStat (ss,l)

transStat (((Call (FunctionCall label args)):ss),l) = ((concatMap transExp args) ++ [SCall label] ++ out,l)
	where (out,_) = transStat (ss,l)
transStat (((Call _):ss),l)             = (out,l)
	where (out,_) = transStat (ss,l)

transStat (((Return exp):ss),l)             = ((transExp exp) ++ [SRet] ++ out,l)
	where (out,_) = transStat (ss,l)

transStat (((LambdaApply label (Var name)):ss),l)   = ([SPushN name] ++ [SCall label] ++ out,l)
	where (out,_) = transStat (ss,l)
transStat (((LambdaApply label (VarArr n e)):ss),l) = ((transExp e) ++ [SGet n] ++ [SCall label] ++ out,l)
	where (out,_) = transStat (ss,l)

{-
transStat (Input (Var name))               =
transStat (Input (VarArr name))            =
-}

-- TODO:
--transStat (((Output (Str s)):ss),l) = [SPrintS s]
--	where name = 
--transStat (((Output exp):ss),l)     = (transExp exp) ++ [SPrintI]

transStat (((LoopUntil cond body):ss),l)   = ([SLabel lbl] ++ bod ++ (transExp cond) ++ [SJTrue lbl] ++ out,l')
	where
		lbl = "L"++(show l)
		(bod,l') = transStat (body,l+1)
		(out,_)  = transStat (ss,l')

transStat (((If cond true false):ss),l)    = ((transExp cond) ++ [SJTrue (lbl++"_true")] ++ [SJump (lbl++"_false")] ++ [SLabel (lbl++"_true")] ++ bodT ++ [SJump (lbl++"_end")] ++ [SLabel (lbl++"_false")] ++ bodF ++ [SLabel (lbl++"_end")] ++ out,l'')
	where
		lbl = "L"++(show l)
		(bodT,l')  = transStat (true,l+1)
		(bodF,l'') = transStat (false,l'+1)
		(out,_)    = transStat (ss,l'')

transStat (((Comment _):ss),l) = (out,l)
	where (out,_) = transStat (ss,l)

transExp :: Exp -> [SInst]
transExp (Int i) = [SPushI i]
transExp (Variable (Var name)) = [SPushN name]
transExp (Variable (VarArr name index)) = (transExp index) ++ [SGet name]
transExp (FunctionCall fn args) = (concatMap transExp args) ++ [SCall fn]
transExp (UnOp op exp) = (transExp exp) ++ (transUnOp op)
transExp (BinOp op exp1 exp2) = (transExp exp1) ++ (transExp exp2) ++ (transOp op)
transExp _ = []

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
transOp LAnd = [SLAnd]
transOp Lt   = [SLt]
transOp Lte  = [SLte]
transOp Gt   = [SGt]
transOp Gte  = [SGte]
