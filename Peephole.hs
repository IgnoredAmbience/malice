module Peephole where
import Types

peephole = map (fixPushPop.removePushXPopX)

fixPushPop :: [MInst] -> [MInst] --takes a push x, pop REG, and turns it into a mov REG x
fixPushPop xs = foldr pushPopToMov [] xs
    where
      pushPopToMov (UnMOp MPush x) ((UnMOp MPop (Reg y)):ys) = (BinMOp MMov (Reg y) x) : ys
      pushPopToMov x ys = x : ys

removePushXPopX :: [MInst] -> [MInst] --takes a push x, pop x, and removes both
removePushXPopX xs = foldr aux [] xs
    where
      aux xIns@(UnMOp MPush x) (yIns@(UnMOp MPop  y):ys) = if x == y then ys else xIns:yIns:ys
      aux xIns@(UnMOp MPop  x) (yIns@(UnMOp MPush y):ys) = if x == y then ys else xIns:yIns:ys
      aux x ys = x : ys