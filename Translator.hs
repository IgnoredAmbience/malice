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
	[SLabel name, SEnter] ++ (getArgs args) ++ [SRestEnter] ++ concatMap transStat stats
	where
	  getArgs :: [(String,Type)] -> [SInst]
          getArgs []             = []
          getArgs ((name,_):as)  = (getArgs as) ++ [SPop name]

transFunc (Lambda name _ stats) = [SLabel name, SEnter, SPop $ name ++ "_it", SRestEnter] ++ concatMap transStat stats'
  where stats' = stats ++ [Return (Variable (Var $ name ++ "_it"))]


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
transStat (LambdaApply label (Var name))   = [SPushN name] ++ [SCall label] ++ [SPop name]
-- TODO Could de-duplicate transExp?
transStat (LambdaApply label (VarArr n e)) = (transExp e) ++ [SGet n] ++ [SCall label] ++ (transExp e) ++ [SPut n]

-- FIXME
transStat (Input (Var name)) = [SInput] ++ [SPop name]
transStat (Input (VarArr name index)) = [SInput] ++ (transExp index) ++ [SPut name]

transStat (Output (Str s)) = ([SPrintS (lblStr s)])
transStat (Output exp ) = ((transExp exp) ++ [SPrintI])

transStat (Return exp ) = ((transExp exp) ++ [SRet])

transStat (LoopUntil cond@(BinOp op lhs rhs) body )
	| elem op comparisons = ([SLabel lblL] ++ (transExp lhs) ++ (transExp rhs) ++ [(transJOp op) lblE] ++ bod ++ [SJump lblL] ++ [SLabel lblE])
	| otherwise           = ([SLabel lblL] ++ [SJTrue lblE] ++ bod ++ (transExp cond) ++ [SJump lblL] ++ [SLabel lblE])
	where
	  (Lbl lbl) = newLabel id
	  lblL = lbl++"_loop"
	  lblE = lbl++"_end"
	  bod = concatMap transStat body
	                       
transStat (LoopUntil cond body ) = ([SLabel lblL] ++ [SJTrue lblE] ++ bod ++ (transExp cond) ++ [SJump lblL] ++ [SLabel lblE])
	where
	  (Lbl lbl) = newLabel id
	  lblL = lbl++"_loop"
	  lblE = lbl++"_end"
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
transExp (UnOp op exp)                  = (transExp exp) ++ (transUnOp op)

-- Short circuit operators have a distinct lack of Jonny 5
transExp (BinOp LOr exp1 exp2)          = (transExp exp1) ++ [SJTrue lblT] ++ (transExp exp2) ++ [SJTrue lblT] ++ [SPushI 0] ++ [SJump lblE] ++ [SLabel lblT] ++ [SPushI 1] ++ [SLabel lblE]
    where
        (Lbl lbl) = newLabel id
        lblT = lbl++"_true"
        lblE = lbl++"_end"
transExp (BinOp LAnd exp1 exp2)         = (transExp exp1) ++ [SJFalse lblF] ++ (transExp exp2) ++ [SJFalse lblF] ++ [SPushI 1] ++ [SJump lblE] ++ [SLabel lblF] ++ [SPushI 0] ++ [SLabel lblE]
    where
        (Lbl lbl) = newLabel id
        lblF = lbl++"_false"
        lblE = lbl++"_end"

transExp (BinOp op exp1 exp2)           = (transExp exp1) ++ (transExp exp2) ++ (transOp op)
transExp (FunctionCall label args)      = (concatMap transExp args) ++ [SCall label] ++ [SPushEax]
transExp _ = error "UNDEFINED EXPRESSION"

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

comparisons = [Eq, Neq, Lt, Lte, Gt, Gte]

transJOp :: BinOp -> String -> SInst
transJOp Eq   = SJEq
transJOp Neq  = SJNeq
transJOp Lt   = SJLt
transJOp Lte  = SJLte
transJOp Gt   = SJGt
transJOp Gte  = SJGte
