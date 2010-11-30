module Semantics where
import Types
import qualified Data.Map as Map

semantics :: Program -> [SymbolTbl]
semantics funcs
  = map (buildFunctionSymbolTbl funcTbl) funcs
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
buildFunctionSymbolTbl :: SymbolTbl -> Function -> SymbolTbl
buildFunctionSymbolTbl funcSt (Function _ _ params stmts)
  = foldl addStatement st stmts
    where
      st = Map.unionWithKey alreadyDefinedError funcSt (Map.fromList params) 
buildFunctionSymbolTbl funcSt (Lambda _ typ stmts)
  = foldl addStatement st stmts
    where
      st = Map.unionWithKey alreadyDefinedError funcSt (Map.singleton "it" typ') 
      typ' = lambdaType stmts typ

-- Build symbol table from a list of statements
stmtsSmbTbl :: SymbolTbl -> [Statement] -> SymbolTbl
stmtsSmbTbl = foldl addStatement

-- Check statement against symbol table, building it as required
addStatement :: SymbolTbl -> Statement -> SymbolTbl 
addStatement st (Declare x t)
  = Map.insertWithKey alreadyDefinedError x t st

addStatement st (DeclareArr name typ size)
  = Map.insertWithKey alreadyDefinedError name (Array typ) st

addStatement st (Assign var exp) 
  | castable vt et = st
  | otherwise      = semError $ "cannot assign expression of type " ++ show et ++ " to variable " ++ show var ++ " of type " ++ show vt
  where
    vt = typeOf st var
    et = expType st exp

addStatement st (Call exp) = checkExp st exp

addStatement st (Increment var)
  | not(isOperatable(typeOf st var)) = semError $ show var ++ " cannot eat."
  | otherwise                        = st

addStatement st (Decrement var)
  | not(isOperatable(typeOf st var)) = semError $ show var ++ " cannot drink."
  | otherwise                        = st

addStatement st (LambdaApply name var)
  | checkLambda lt vt = semError $ show var ++ " (" ++ show vt ++ ") cannot go through the looking-glass " ++ show name ++ " (" ++ show lt ++ ")."
  | otherwise= st
  where
    LambdaType lt = Map.findWithDefault (undefinedLambError name) name st
    vt            = typeOf st var

addStatement st (Input var)
  | not(isOperatable(typeOf st var)) = semError $ show var ++ " cannot be input."
  | otherwise                        = st

addStatement st (Output exp)
  | isPrintable $ expType st exp = st
  | otherwise                    = semError $ show exp ++ " is not printable."

addStatement st (Return exp) = checkExp st exp

-- Fold loops and expressions back into global symbol table... Not exactly as its supposed to be done!
addStatement st (LoopUntil exp sts) = stmtsSmbTbl st' sts
  where st' = checkExp st exp

addStatement st (If exp thenSts elseSts) = stmtsSmbTbl st'' elseSts
  where
    st'  = checkExp st exp
    st'' = stmtsSmbTbl st' thenSts
addStatement st (Comment _) = st

expType :: SymbolTbl -> Exp -> Type
expType st = fst . semanticExp st

checkExp :: SymbolTbl -> Exp -> SymbolTbl
checkExp st = snd . semanticExp st

semanticExp :: SymbolTbl -> Exp -> (Type, SymbolTbl)
semanticExp st (Int _)  = (Number, st)
semanticExp st (Char _) = (Letter, st)
semanticExp st (Str _)  = (Sentence, st)
semanticExp st (Variable v) = (typeOf st v, st)

semanticExp st (UnOp op exp)    
  | isOperatable t = subExp
  | otherwise      = typeInoperableError exp t op
  where subExp@(t, _) = semanticExp st exp

semanticExp st (BinOp op e1 e2)
  | not(isOperatable e1t) = typeInoperableError e1 e1t op
  | not(isOperatable e2t) = typeInoperableError e2 e2t op
  | e1t /= e2t        = castWarning e1t e2t op
  | otherwise         = (e1t, st)
  where
    e1t = expType st e1
    e2t = expType st e2

semanticExp st (FunctionCall name params)
  | and comparedTypes = (t, st)
  | otherwise         = funcParamMismatch name expParamTypes paramTypes
  where
    paramTypes = map (expType st) params
    FunctionType t expParamTypes = Map.findWithDefault (undefinedFuncError name) name st
    comparedTypes = zipWith castable paramTypes expParamTypes

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
castable x y
  | x == y    = True
  | otherwise = False

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

-- Error messages
semError = error . (++) "Semantic error: " 
alreadyDefinedError k old new = semError $ "cannot redefine " ++ show k ++ " as a " ++ show new ++ ", already defined as a " ++ show old ++ "."
undefinedError x = semError $ "use of undefined variable " ++ show x
undefinedFuncError x = semError $ "use of undefined room " ++ show x
funcParamMismatch r x y = semError $ "mismatch of types of parameters passed into the room " ++ show r ++ ", expected " ++ show x ++ " but got " ++ show y ++ "."
typeInoperableError exp t op = semError $ "subexpression of type " ++ show t ++ " not compatible with " ++ show op ++ ". (Subexpression was: " ++ show exp ++ ")"
undefinedLambError x = semError $ "use of undefined looking-glass " ++ show x

--semWarning = "Semantic warning: "
castWarning t t2 op = semError $ show t ++ " and " ++ show t2 ++ " not directly compatible over " ++ show op ++ " one will be recast."
