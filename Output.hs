module Output where
import Types

outputASM :: [Inst] -> [String]
outputASM = map toASM

toASM :: Inst -> String
