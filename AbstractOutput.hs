module AbstractOutput where
import Types
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (intercalate)

abstract :: [[SInst]] -> [[MInst]]
abstract = map $ concatMap toSymbInstr

toSymbInstr :: SInst -> [MInst]
toSymbInstr SOr  = [UnMOp MPop (Reg EAX), BinMOp MOr  (Indirect (Reg ESP)) (Reg EAX)]
toSymbInstr SAnd = [UnMOp MPop (Reg EAX), BinMOp MAnd (Indirect (Reg ESP)) (Reg EAX)]
toSymbInstr SXor = [UnMOp MPop (Reg EAX), BinMOp MXor (Indirect (Reg ESP)) (Reg EAX)]
toSymbInstr SAdd = [UnMOp MPop (Reg EAX), BinMOp MAdd (Indirect (Reg ESP)) (Reg EAX)]
toSymbInstr SSub = [UnMOp MPop (Reg EAX), BinMOp MSub (Indirect (Reg ESP)) (Reg EAX)]
toSymbInstr SMul = [UnMOp MPop (Reg EAX), BinMOp MMul (Reg EAX) (Indirect (Reg ESP)), BinMOp MMov (Indirect (Reg ESP)) (Reg EAX)]
toSymbInstr SDiv = [UnMOp MPop (Reg EBX), UnMOp MPop (Reg EAX), BinMOp MXor (Reg EDX) (Reg EDX), UnMOp MDiv (Reg EBX), UnMOp MPush (Reg EAX)]
toSymbInstr SMod = [UnMOp MPop (Reg EBX), UnMOp MPop (Reg EAX), BinMOp MXor (Reg EDX) (Reg EDX), UnMOp MDiv (Reg EBX), UnMOp MPush (Reg EDX)]


toSymbInstr SLOr  = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX)] -- EAX || EBX
toSymbInstr SLAnd = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX)] -- EAX && EBX
toSymbInstr SEq   = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX),JmpMOp MJE label,BinMOp MMov (Reg EAX) (Const 0), UnMOp MPush (Reg EAX), Label label, BinMOp MMov (Reg EAX) (Const 1), UnMOp MPush (Reg EAX)] -- EAX == EBX
    where label = newLabel id
toSymbInstr SNeq = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX),JmpMOp MJNE label, BinMOp MMov (Reg EAX) (Const 0), UnMOp MPush (Reg EAX), Label label, BinMOp MMov (Reg EAX) (Const 1), UnMOp MPush (Reg EAX)] -- EAX != EBX
    where label = newLabel id
toSymbInstr SLt   = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX),JmpMOp MJL label, BinMOp MMov (Reg EAX) (Const 0), UnMOp MPush (Reg EAX), Label label, BinMOp MMov (Reg EAX) (Const 1), UnMOp MPush (Reg EAX)] -- EAX < EBX
    where label = newLabel id
toSymbInstr SLte  = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX),JmpMOp MJLE label, BinMOp MMov (Reg EAX) (Const 0), UnMOp MPush (Reg EAX), Label label, BinMOp MMov (Reg EAX) (Const 1), UnMOp MPush (Reg EAX)] -- EAX <= EBX
    where label = newLabel id
toSymbInstr SGt   = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX),JmpMOp MJG label, BinMOp MMov (Reg EAX) (Const 0), UnMOp MPush (Reg EAX), Label label, BinMOp MMov (Reg EAX) (Const 1), UnMOp MPush (Reg EAX)] -- EAX > EBX
    where label = newLabel id
toSymbInstr SGte  = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX),JmpMOp MJGE label, BinMOp MMov (Reg EAX) (Const 0), UnMOp MPush (Reg EAX), Label label, BinMOp MMov (Reg EAX) (Const 1), UnMOp MPush (Reg EAX)] -- EAX >= EBX
    where label = newLabel id


toSymbInstr (SJLOr  label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MLOr (Reg EAX) (Reg EBX), BinMOp MCmp (Reg EAX) (Const 0), JmpMOp MJNE (Lbl label) ]
toSymbInstr (SJLAnd label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), UnMOp MNot (Reg EAX), UnMOp MNot (Reg EBX), BinMOp MLOr (Reg EAX) (Reg EBX), BinMOp MCmp (Reg EAX) (Const 0), JmpMOp MJE (Lbl label) ]
toSymbInstr (SJEq  label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX), JmpMOp MJE  (Lbl label)]
toSymbInstr (SJNeq label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX), JmpMOp MJNE (Lbl label)]
toSymbInstr (SJLt  label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX), JmpMOp MJL  (Lbl label)]
toSymbInstr (SJLte label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX), JmpMOp MJLE (Lbl label)]
toSymbInstr (SJGt  label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX), JmpMOp MJG  (Lbl label)]
toSymbInstr (SJGte label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EAX) (Reg EBX), JmpMOp MJGE (Lbl label)]


toSymbInstr SNot = [UnMOp MNot (DWord (Indirect (Reg ESP)))]
toSymbInstr SNeg = [UnMOp MNeg (DWord (Indirect (Reg ESP)))]
toSymbInstr SInc = [UnMOp MInc (DWord (Indirect (Reg ESP)))]
toSymbInstr SDec = [UnMOp MDec (DWord (Indirect (Reg ESP)))]

-- TODO
toSymbInstr (SPrintS _) = []
toSymbInstr SPrintI = [UnMOp MPop (Reg EAX), JmpMOp MCall (Lbl "output_int")]

toSymbInstr (SPushI i) = [UnMOp MPush (Const i)]
toSymbInstr (SPushN n) = [UnMOp MPush (Name n)]
toSymbInstr (SPop n) = [UnMOp MPop (Name n)]

toSymbInstr (SGet n) = [UnMOp MPop (Reg EAX), BinMOp MMov (Reg EBX) (Name n) -- Get the index and name
                     , BinMOp MMov (Reg EAX) (IndirectScale (Reg EBX) Four (Reg EAX)), UnMOp MPush (Reg EAX)] -- Get the value itself and push it

toSymbInstr (SPut n) = [UnMOp MPop (Reg EAX), BinMOp MMov (Reg EBX) (Name n), UnMOp MPop (Reg ECX) -- Get the value, name and index
                     , BinMOp MMov (IndirectScale (Reg EBX) Four (Reg EAX)) (Reg ECX)] -- Put the value

toSymbInstr (SJump label)  = [JmpMOp MJmp (Lbl label)]
toSymbInstr (SJTrue label) = [UnMOp MPop (Reg EAX), BinMOp MCmp (Reg EAX) (Const 0), JmpMOp MJNE (Lbl label)]
toSymbInstr (SCall label)  = [JmpMOp MCall (Lbl label)]
toSymbInstr (SRet)         = [UnMOp MPop (Reg EAX), NonMOp MRet]
toSymbInstr (SLabel s)     = [Label (Lbl s)]

instance Show SInst where --moved here from Types.hs to prevent circular dependencies. Cleanest way to do it
    show = intercalate "\n" . map show .toSymbInstr 

--ninja hacks. unsafePerformIO necessary, and none side effecting (besides the +1 each time)
counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

newLabel :: a -> Lbl
newLabel _ = unsafePerformIO $
             do
               i <- readIORef counter
               writeIORef counter (i+1)
               return . Lbl $ "label" ++ show i
