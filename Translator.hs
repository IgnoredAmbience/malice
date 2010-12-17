module Translator(translate) where
import System.IO.Unsafe
import Data.IORef
import Data.Char
import Types

-- Translates statements/expressions/etc into a list of abstract Instructions
translate :: Program -> [[SInst]]
translate functions = map transFunc functions

transFunc :: Function -> [SInst]
transFunc (Function name _ args stats) =
	[SLabel name] ++ (popArgs args) ++ concatMap transStat stats
	where
	  popArgs :: [(String,Type)] -> [SInst]
          popArgs []                  = []
          popArgs ((name,Number):as)  = (popArgs as) ++ [SPushN name]
	  popArgs ((name,Array _):as) = (popArgs as) ++ [SPushN name] -- TODO: make sure this actually works


transStat :: Statement -> [SInst]
transStat (Declare _ _) = []
transStat (DeclareArr name _ length)       = (transExp length) ++ [SPushI 0] ++ [SPut name]
transStat (Assign (Var name) exp)          = (transExp exp) ++ [SPop name]
transStat (Assign (VarArr name index) exp) = (transExp exp) ++ (transExp index) ++ [SPut name]
transStat (Call (FunctionCall label args)) = (concatMap transExp args) ++ [SCall label]
transStat (Call _)                         = []
transStat (Increment (Var name))           = [SPushN name] ++ [SInc] ++ [SPop name]
transStat (Increment (VarArr name index))  = (transExp index) ++ [SGet name] ++ [SInc] ++ (transExp index) ++ [SPut name]
transStat (Decrement (Var name))           = [SPushN name] ++ [SDec] ++ [SPop name]
transStat (Decrement (VarArr name index))  = (transExp index) ++ [SGet name] ++ [SDec] ++ (transExp index) ++ [SPut name]
transStat (LambdaApply label (Var name))   = [SPushN name] ++ [SCall label]
transStat (LambdaApply label (VarArr n e)) = (transExp e) ++ [SGet n] ++ [SCall label]

-- FIXME
transStat (Input (Var name))        = error "Input (Var name) is undefined"
transStat (Input (VarArr name exp)) = error "Input (VarArr name exp) is undefined"

-- TODO:
transStat (Output (Str s)) = ([SPrintS (lblStr s)])
transStat (Output exp ) = ((transExp exp) ++ [SPrintI])
transStat (Return exp ) = ((transExp exp) ++ [SRet])


transStat (LoopUntil cond@(BinOp op lhs rhs) body )
	| elem op comparisons = ([SLabel lbl] ++ bod ++ (transExp lhs) ++ (transExp rhs) ++ [transJOp op lbl])
	| otherwise           = ([SLabel lbl] ++ bod ++ (transExp cond) ++ [SJTrue lbl])
	where
	  (Lbl lbl) = newLabel id
	  bod = concatMap transStat body
	                       
transStat (LoopUntil cond body ) = ([SLabel lbl] ++ bod ++ (transExp cond) ++ [SJTrue lbl])
	where
	  (Lbl lbl) = newLabel id
	  bod = concatMap transStat body

transStat ((If cond@(BinOp op lhs rhs) true false) )
	| elem op comparisons = ((transExp lhs) ++ (transExp rhs) ++ [(transJOp op) lblT] ++ [SJump lblF]
							++ [SLabel lblT] ++ bodT ++ [SJump lblE]
							++ [SLabel lblF] ++ bodF ++ [SLabel lblE])
	| otherwise           = ((transExp cond) ++ [SJTrue lblT] ++ [SJump lblF]
							++ [SLabel lblT] ++ bodT ++ [SJump lblE]
							++ [SLabel lblF] ++ bodF ++ [SLabel lblE])
	where
		(Lbl lbl) = newLabel id
		lblT = lbl ++ "_true"
		lblF = lbl ++ "_false"
		lblE = lbl ++ "_end"
		bodT = concatMap transStat true
		bodF = concatMap transStat false

transStat (If cond true false ) = ((transExp cond) ++ [SJTrue lblT] ++ [SJump lblF]
                                  ++ [SLabel lblT] ++ bodT ++ [SJump lblE]
                                  ++ [SLabel lblF] ++ bodF ++ [SLabel lblE])
	where
		(Lbl lbl) = newLabel id
		lblT = lbl ++ "_true"
		lblF = lbl ++ "_false"
		lblE = lbl ++ "_end"
		bodT = concatMap transStat true
		bodF = concatMap transStat false
		

transStat (Comment _ ) = []

transStat _  = error "UNDEFINED STATEMENT"
	

transExp :: Exp -> [SInst]
transExp (Int i)                        = [SPushI i]
transExp (Char c)                       = [SPushI (ord c)]
transExp (Variable (Var name))          = [SPushN name]
transExp (Variable (VarArr name index)) = (transExp index) ++ [SGet name]
transExp (FunctionCall fn args)         = (concatMap transExp args) ++ [SCall fn]
transExp (UnOp op exp)                  = (transExp exp) ++ (transUnOp op)
transExp (BinOp op exp1 exp2)           = (transExp exp1) ++ (transExp exp2) ++ (transOp op)

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
transOp Eq   = [SEq]
transOp Neq  = [SNeq]
transOp Lt   = [SLt]
transOp Lte  = [SLte]
transOp Gt   = [SGt]
transOp Gte  = [SGte]

counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

newLabel :: a -> Lbl
newLabel _ = unsafePerformIO $
             do
               i <- readIORef counter
               writeIORef counter (i+1)
               return . Lbl $ "L" ++ show i

comparisons = [LOr, LAnd, Eq, Neq, Lt, Lte, Gt, Gte]

transJOp :: BinOp -> String -> SInst
transJOp LOr  = SJLOr
transJOp LAnd = SJLAnd
transJOp Eq   = SJEq
transJOp Neq  = SJNeq
transJOp Lt   = SJLt
transJOp Lte  = SJLte
transJOp Gt   = SJGt
transJOp Gte  = SJGte
