module Semantics where
import Types
import qualified Data.Map as Map
import Data.HashTable (hashString)

type Rewrite = Map.Map String String

semantics :: Program -> (Program, [SymbolTbl], DataTbl)
semantics funcs
  = unzip $ map (buildFunctionSymbolTbl (funcTbl, Map.empty)) funcs
    where
      funcTbl = foldl addFunction Map.empty funcs

-- Add function definitions into (global) symbol table
addFunction :: SymbolTbl -> Function -> SymbolTbl
addFunction st (Function name typ ps _)
  -- Will attempt insertion, calling alreadyDefinedError if a conflict arises
  = Map.insertWithKey alreadyDefinedError name (FunctionType typ paramTypes) st
    where
      paramTypes = map snd ps

addFunction st (Lambda name typ _)
  = Map.insertWithKey alreadyDefinedError name (LambdaType typ) st

-- Build symbol table for a function
buildFunctionSymbolTbl :: (SymbolTbl, DataTbl) -> Function -> (Function, SymbolTbl, DataTbl)
buildFunctionSymbolTbl (funcSt, dt) (Function name typ params stmts)
  = (Function name typ params stmts', st', dt')
    where
      (stmts', st', dt') = foldl (addStatement 0) ([], st, Map.empty, dt) stmts
      st = Map.unionWithKey alreadyDefinedError funcSt (Map.fromList params) 

buildFunctionSymbolTbl (funcSt, dt) (Lambda name typ stmts)
  = (Lambda name typ stmts', st', dt')
    where
      (stmts', st', _, dt') = foldl (addStatement 0) ([], st, Map.empty, dt) stmts
      st = Map.unionWithKey alreadyDefinedError funcSt (Map.singleton "it" typ') 
      typ' = lambdaType stmts typ

-- Build symbol table from a list of statements
stmtsSmbTbl :: Int -> ([Statement], SymbolTbl, Rewrite, DataTbl) -> [Statement] -> ([Statement], SymbolTbl, Rewrite, DataTbl)
stmtsSmbTbl d = foldl (addStatement d)

-- Check statement against symbol table, building it as required
addStatement :: Int -> ([Statement], SymbolTbl, Rewrite, DataTbl) -> Statement -> ([Statement], SymbolTbl, Rewrite, DataTbl)
addStatement d (ss,st,rt,dt) s@(Declare x t)
  -- Symbol already in table, and at root level or has already been rewritten to new var
  | member && (d == 0 || rewritten) = alreadyDefinedError x (st Map.! x) t
  -- Symbol already in table, but at a higher scope, rename into symbtbl, add alias to rename table
  | member           = (ss++[Declare newName t], Map.insert newName t st, Map.insert x newName rt)
  | otherwise        = (ss++[s], Map.insert x t st, rt, dt)
  where
    member = Map.member x st
    newName = x ++ "_" ++ show d
    rewritten = (Map.member x rt) && (rt Map.! x == newName)

addStatement d (ss,st,rt,dt) s@(DeclareArr name typ size)
  -- Symbol already in table, and at root level or has already been rewritten to new var
  | member && (d == 0 || rewritten) = alreadyDefinedError name (st Map.! name) (Array typ)
  -- Symbol already in table, but at a higher scope, rename into symbtbl, add alias to rename table
  | member           = (ss++[DeclareArr newName typ size], Map.insert newName (Array typ) st, Map.insert name newName rt)
  | otherwise        = (ss++[s], Map.insert name (Array typ) st, rt, dt)
  where
    member = Map.member name st
    newName = name ++ "_" ++ show d
    rewritten = (Map.member name rt) && (rt Map.! name == newName)

addStatement _ (ss,st,rt,dt) (Assign var exp)
  | castable vt et = (ss++[Assign var' exp'], st, rt, dt')
  | otherwise      = semError $ "cannot assign expression of type " ++ show et ++ " to variable " ++ show var ++ " of type " ++ show vt
  where
    exp' = renameExpVars rt exp
    var' = renameVar rt var
    vt = typeOf st var'
    (et, dt') = semanticExp (st, dt) exp'

addStatement _ (ss,st,rt,dt) (Call exp) = (ss++[Call exp'], st, rt, dt')
  where
    exp' = renameExpVars rt exp
    (_, dt') = semanticExp (st, dt) exp'

addStatement _ (ss,st,rt,dt) (Increment var)
  | not(isOperatable(typeOf st var')) = semError $ show var ++ " cannot eat."
  | otherwise                         = (ss++[Increment var'], st, rt, dt)
    where
      var' = renameVar rt var

addStatement _ (ss,st,rt,dt) (Decrement var)
  | not(isOperatable(typeOf st var')) = semError $ show var ++ " cannot drink."
  | otherwise                         = (ss++[Decrement var'], st, rt, dt)
    where
      var' = renameVar rt var

addStatement _ (ss,st,rt,dt) (LambdaApply name var)
  | checkLambda lt vt = semError $ show var ++ " (" ++ show vt ++ ") cannot go through the looking-glass " ++ show name ++ " (" ++ show lt ++ ")."
  | otherwise = (ss++[LambdaApply name var'], st, rt, dt)
  where
    LambdaType lt = Map.findWithDefault (undefinedLambError name) name st
    vt            = typeOf st var'
    var'          = renameVar rt var

addStatement _ (ss,st,rt,dt) (Input var)
  | not(isOperatable(typeOf st var')) = semError $ show var ++ " cannot be input."
  | otherwise                         = (ss++[Input var'], st, rt, dt)
  where
    var' = renameVar rt var

addStatement _ (ss,st,rt,dt) (Output exp)
  | isPrintable t = (ss++[Output exp'], st, rt, dt')
  | otherwise     = semError $ show exp ++ " is not printable."
    where
      exp'     = renameExpVars rt exp
      (t, dt') = semanticExp (st, dt) exp'

addStatement _ (ss,st,rt,dt) (Return exp) = (ss++[Return exp'], st, rt, dt')
  where
    exp'     = renameExpVars rt exp
    (_, dt') = semanticExp (st, dt) exp'

addStatement d (ss,st,rt,dt) (LoopUntil exp sts) = (ss++[LoopUntil exp' loopStats], st'', rt, dt')
  where
    (loopStats, st'', _) = stmtsSmbTbl (d+1) ([], st, rt) sts
    exp'      = renameExpVars rt exp
    (_, dt')  = semanticExp (st, dt) exp'

addStatement d (ss,st,rt,dt) (If exp thenSts elseSts) = (ss++[If exp' thenSts' elseSts'], st''', rt, dt)
  where
    exp'      = renameExpVars rt exp
    (_, dt')  = semanticExp (st, dt) exp'
    (thenSts', st'', _) = stmtsSmbTbl (d+1) ([], st, rt) thenSts
    (elseSts', st''', _) = stmtsSmbTbl (d+1) ([], st'', rt) elseSts

addStatement _ st (Comment _) = st

--expType :: (SymbolTbl, DataTbl) -> Exp -> Type
--expType st = fst . semanticExp st
--
--checkExp :: SymbolTbl -> Exp -> SymbolTbl
--checkExp st = snd . semanticExp st

semanticExp :: (SymbolTbl, DataTbl) -> Exp -> (Type, DataTbl)
semanticExp (st,dt) (Int _)  = (Number, dt)
semanticExp (st,dt) (Char _) = (Letter, dt)
semanticExp (st,dt) (Str s)  = (Sentence, dt')
  where dt' = Map.insert s (lblStr s)
semanticExp (st,dt) (Variable v) = (typeOf st v, dt)

semanticExp (st,dt) (UnOp op exp)    
  | isOperatable t = subExp
  | otherwise      = typeInoperableError exp t op
  where subExp@(t, _) = semanticExp (st,dt) exp

semanticExp (st,dt) (BinOp op e1 e2)
  | not(isOperatable e1t) = typeInoperableError e1 e1t op
  | not(isOperatable e2t) = typeInoperableError e2 e2t op
  | e1t /= e2t        = ({-castWarning e1t e2t op-} e1t, dt'')
  | otherwise         = (e1t, dt'')
  where
    (e1t,dt')  = semanticExp (st, dt) e1
    (e2t,dt'') = semanticExp (st, dt') e2

semanticExp (st,dt) (FunctionCall name params)
  | and comparedTypes = (t, dt')
  | otherwise         = funcParamMismatch name expParamTypes paramTypes
  where
    (paramTypes, dt') = checkParamTypes params st dt
    FunctionType t expParamTypes = Map.findWithDefault (undefinedFuncError name) name st
    comparedTypes = zipWith castable paramTypes expParamTypes

checkParamTypes :: [Exp] -> SymbolTbl -> DataTbl -> ([Type], DataTbl)
checkParamTypes [] d = ([], d)
checkParamTypes s (e:es) d = (t:ts, d'')
  where
    (ts, d'') = checkParamTypes s es d'
    (t, d')   = semanticExp (s, d) e

renameExpVars :: Rewrite -> Exp -> Exp
renameExpVars rt (UnOp o x)          = UnOp o (renameExpVars rt x)
renameExpVars rt (BinOp o x1 x2)     = BinOp o (renameExpVars rt x1) (renameExpVars rt x2)
renameExpVars rt (FunctionCall n xs) = FunctionCall n (map (renameExpVars rt) xs)
renameExpVars rt (Variable v)        = Variable (renameVar rt v)
renameExpVars _ x                    = x

renameVar :: Rewrite -> Variable -> Variable
renameVar rt (Var n)      = Var (Map.findWithDefault n n rt)
renameVar rt (VarArr n x) = VarArr (Map.findWithDefault n n rt) (renameExpVars rt x)

-- Axioms
isPrintable :: Type -> Bool
isPrintable Number = True
isPrintable Letter = True
isPrintable Sentence = True
isPrintable _ = False

isOperatable :: Type -> Bool
isOperatable Number = True
isOperatable Letter = True
isOperatable _  = False

castable :: Type -> Type -> Bool
castable Number Letter = True -- castWarning
castable Letter Number = True
castable x y = x == y

checkLambda :: Type -> Type -> Bool
checkLambda lt (Array vt) = lt /= vt
checkLambda lt vt         = lt /= vt

lambdaType :: [Statement] -> Type -> Type
lambdaType s t
  | lambdaIsArray s = Array t
  | otherwise       = t

lambdaIsArray :: [Statement] -> Bool
lambdaIsArray = any lambdaIsArray'

lambdaIsArray' :: Statement -> Bool
lambdaIsArray' (Assign (VarArr "it" _) _) = True
lambdaIsArray' (Increment (VarArr "it" _)) = True
lambdaIsArray' (Decrement (VarArr "it" _)) = True
lambdaIsArray' (LambdaApply _ (VarArr "it" _)) = True
lambdaIsArray' (Input (VarArr "it" _)) = True
lambdaIsArray' (LoopUntil _ s) = lambdaIsArray s
lambdaIsArray' (If _ s1 s2) = lambdaIsArray s1 || lambdaIsArray s2
lambdaIsArray' _ = False

typeOf :: SymbolTbl -> Variable -> Type
typeOf st (Var x)      = Map.findWithDefault (undefinedError x) x st
typeOf st (VarArr x _) = t
  where Array t = Map.findWithDefault (undefinedError x) x st

lblStr s = "str" ++ show (hashString s)

-- Error messages
semError = error . (++) "Semantic error: " 
alreadyDefinedError k old new = semError $ "cannot redefine " ++ show k ++ " as a " ++ show new ++ ", already defined as a " ++ show old ++ "."
undefinedError x = semError $ "use of undefined variable " ++ show x
undefinedFuncError x = semError $ "use of undefined room " ++ show x
funcParamMismatch r x y = semError $ "mismatch of types of parameters passed into the room " ++ show r ++ ", expected " ++ show x ++ " but got " ++ show y ++ "."
typeInoperableError exp t op = semError $ "subexpression of type " ++ show t ++ " not compatible with " ++ show op ++ ". (Subexpression was: " ++ show exp ++ ")"
undefinedLambError x = semError $ "use of undefined looking-glass " ++ show x

--semWarning = "Semantic warning: "
--castWarning t t2 op = semError $ show t ++ " and " ++ show t2 ++ " not directly compatible over " ++ show op ++ " one will be recast."
