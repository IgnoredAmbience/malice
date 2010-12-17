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
	[SLabel name, SEnter] ++ (getArgs args) ++ [SRestEnter] ++ concatMap (transStat args) stats
	where
	  getArgs :: [(String,Type)] -> [SInst]
          getArgs []             = []
          getArgs ((name,_):as)  = (getArgs as) ++ [SPop name]

transFunc (Lambda name _ stats) = [SLabel name, SEnter, SPop $ name ++ "_it", SRestEnter] ++ concatMap (transStat []) stats'
  where stats' = stats ++ [Return (Variable (Var $ name ++ "_it"))]

transStat :: [(String, Type)] -> Statement -> [SInst]
transStat _ (Declare _ _)                    = []
transStat a (DeclareArr name _ length)       = (transExp a length) ++ [SPushI 0] ++ [SPut name]
transStat a (Assign (Var name) exp)          = (transExp a exp) ++ [SPop name]
transStat a (Assign (VarArr name index) exp) = (transExp a exp) ++ (transExp a index) ++ [SPut name]
transStat a (Call (FunctionCall label args)) = pushVars a ++ (concatMap (transExp a) args) ++ [SCall label] ++ popVars a
transStat _ (Call _)                         = []
transStat _ (Increment (Var name))           = [SPushN name] ++ [SInc] ++ [SPop name]
transStat a (Increment (VarArr name index))  = (transExp a index) ++ [SGet name] ++ [SInc] ++ (transExp a index) ++ [SPut name]
transStat _ (Decrement (Var name))           = [SPushN name] ++ [SDec] ++ [SPop name]
transStat a (Decrement (VarArr name index))  = (transExp a index) ++ [SGet name] ++ [SDec] ++ (transExp a index) ++ [SPut name]
transStat _ (LambdaApply label (Var name))   = [SPushN name] ++ [SCall label] ++ [SPushEax] ++ [SPop name]
-- TODO Could de-duplicate transExp?
transStat a (LambdaApply label (VarArr n e)) = (transExp a e) ++ [SGet n] ++ [SCall label] ++ (transExp a e) ++ [SPut n]

-- FIXME
transStat _ (Input (Var name)) = [SInput] ++ [SPop name]
transStat a (Input (VarArr name index)) = [SInput] ++ (transExp a index) ++ [SPut name]

transStat _ (Output (Str s)) = ([SPrintS (lblStr s)])
transStat a (Output exp ) = ((transExp a exp) ++ [SPrintI])

transStat a (Return exp ) = ((transExp a exp) ++ [SRet])

transStat args (LoopUntil cond@(BinOp op lhs rhs) body )
	| elem op comparisons = ([SLabel lblL] ++ (transExp args lhs) ++ (transExp args rhs) ++ [(transJOp op) lblE] ++ bod ++ [SJump lblL] ++ [SLabel lblE])
	| otherwise           = ([SLabel lblL] ++ [SJTrue lblE] ++ bod ++ (transExp args cond) ++ [SJump lblL] ++ [SLabel lblE])
	where
	  (Lbl lbl) = newLabel id
	  lblL = lbl++"_loop"
	  lblE = lbl++"_end"
	  bod = concatMap (transStat args) body
	                       
transStat args (LoopUntil cond body ) = ([SLabel lblL] ++ [SJTrue lblE] ++ bod ++ (transExp args cond) ++ [SJump lblL] ++ [SLabel lblE])
	where
	  (Lbl lbl) = newLabel id
	  lblL = lbl++"_loop"
	  lblE = lbl++"_end"
	  bod = concatMap (transStat args) body

transStat args ((If cond@(BinOp op lhs rhs) true false) )
	| elem op comparisons = ((transExp args lhs) ++ (transExp args rhs) ++ [(transJOp op) lblT] ++ [SJump lblF]
							++ [SLabel lblT] ++ bodT ++ [SJump lblE]
							++ [SLabel lblF] ++ bodF ++ [SLabel lblE])
	| otherwise           = ((transExp args cond) ++ [SJTrue lblT] ++ [SJump lblF]
							++ [SLabel lblT] ++ bodT ++ [SJump lblE]
							++ [SLabel lblF] ++ bodF ++ [SLabel lblE])
	where
		(Lbl lbl) = newLabel id
		lblT = lbl ++ "_true"
		lblF = lbl ++ "_false"
		lblE = lbl ++ "_end"
		bodT = concatMap (transStat args) true
		bodF = concatMap (transStat args) false

transStat args (If cond true false ) = ((transExp args cond) ++ [SJTrue lblT] ++ [SJump lblF]
                                  ++ [SLabel lblT] ++ bodT ++ [SJump lblE]
                                  ++ [SLabel lblF] ++ bodF ++ [SLabel lblE])
	where
		(Lbl lbl) = newLabel id
		lblT = lbl ++ "_true"
		lblF = lbl ++ "_false"
		lblE = lbl ++ "_end"
		bodT = concatMap (transStat args) true
		bodF = concatMap (transStat args) false
		

transStat _ (Comment _ ) = []

transStat _ x  = error ("UNDEFINED STATEMENT: " ++ show x)
	
transExp :: [(String, Type)] -> Exp -> [SInst]
transExp _ (Int i)                        = [SPushI i]
transExp _ (Char c)                       = [SPushI (ord c)]
transExp _ (Str _)                        = [SPushI 0]
transExp _ (Variable (Var name))          = [SPushN name]
transExp a (Variable (VarArr name index)) = (transExp a index) ++ [SGet name]
transExp a (UnOp op exp)                  = (transExp a exp) ++ (transUnOp op)

-- Short circuit operators have a distinct lack of Jonny 5
transExp a (BinOp LOr exp1 exp2)          = (transExp a exp1) ++ [SJTrue lblT] ++ (transExp a exp2) ++ [SJTrue lblT] ++ [SPushI 0] ++ [SJump lblE] ++ [SLabel lblT] ++ [SPushI 1] ++ [SLabel lblE]
    where
        (Lbl lbl) = newLabel id
        lblT = lbl++"_true"
        lblE = lbl++"_end"

transExp a (BinOp LAnd exp1 exp2)         = (transExp a exp1) ++ [SJFalse lblF] ++ (transExp a exp2) ++ [SJFalse lblF] ++ [SPushI 1] ++ [SJump lblE] ++ [SLabel lblF] ++ [SPushI 0] ++ [SLabel lblE]
    where
        (Lbl lbl) = newLabel id
        lblF = lbl++"_false"
        lblE = lbl++"_end"

transExp a (BinOp op exp1 exp2)           = (transExp a exp1) ++ (transExp a exp2) ++ (transOp op)

transExp a (FunctionCall label args)      = pushVars a ++ (concatMap (transExp a) args) ++ [SCall label] ++ popVars a ++ [SPushEax]
transExp _ x = error ("UNDEFINED EXPRESSION: " ++ show x)

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

pushVars a = map (\(name,_) -> SPushN name) a
popVars  a = map (\(name,_) -> SPop name) (reverse a)

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
