module Peephole where
import Types
import Debug.Trace

peephole = map removePushPop

removePushPop :: [MInst] -> [MInst] --given x:y, remove both if removePushPop' x y
removePushPop xs@(_:xs') = map fst $ filter snd $ zip xs ((foldr propagateFalse [] $ zipWith removePushPop' xs xs')++ [True])
removePushPop [] = []

removePushPop' :: MInst -> MInst -> Bool
removePushPop' (UnMOp MPush x) (UnMOp MPop y)  = x /= y
removePushPop' (UnMOp MPop x)  (UnMOp MPush y) = x /= y
removePushPop'  _               _              = True

propagateFalse :: Bool -> [Bool] -> [Bool] --if x is true then the value after x should also be true i.e if there is a push then a pop, then remove both x and the one 
propagateFalse x (y:ys) = x : (if not y then False else x) : ys
propagateFalse x []     = [x]


