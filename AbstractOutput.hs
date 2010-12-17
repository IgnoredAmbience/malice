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

toSymbInstr (SShiftL i) = [BinMOp MShl (Indirect (Reg ESP)) (Const i)]
toSymbInstr (SShiftR i) = [BinMOp MShr (Indirect (Reg ESP)) (Const i)]


toSymbInstr SEq = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX)
                   , JmpMOp MJE lblT, UnMOp MPush (Const 0), JmpMOp MJmp lblE
                   , Label lblT, UnMOp MPush (Const 1), Label lblE] -- EAX > EBX
    where
        (Lbl lbl) = newLabel id
        lblT = (Lbl (lbl++"_true"))
        lblE = (Lbl (lbl++"_end"))

toSymbInstr SNeq = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX)
                   , JmpMOp MJNE lblT, UnMOp MPush (Const 0), JmpMOp MJmp lblE
                   , Label lblT, UnMOp MPush (Const 1), Label lblE] -- EAX > EBX
    where
        (Lbl lbl) = newLabel id
        lblT = (Lbl (lbl++"_true"))
        lblE = (Lbl (lbl++"_end"))

toSymbInstr SLt = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX)
                  , JmpMOp MJL lblT, UnMOp MPush (Const 0), JmpMOp MJmp lblE
                  , Label lblT, UnMOp MPush (Const 1), Label lblE] -- EAX > EBX
    where
        (Lbl lbl) = newLabel id
        lblT = (Lbl (lbl++"_true"))
        lblE = (Lbl (lbl++"_end"))

toSymbInstr SLte = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX)
                   , JmpMOp MJLE lblT, UnMOp MPush (Const 0), JmpMOp MJmp lblE
                   , Label lblT, UnMOp MPush (Const 1), Label lblE] -- EAX > EBX
    where
        (Lbl lbl) = newLabel id
        lblT = (Lbl (lbl++"_true"))
        lblE = (Lbl (lbl++"_end"))

toSymbInstr SGt = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX)
                  , JmpMOp MJG lblT, UnMOp MPush (Const 0), JmpMOp MJmp lblE
                  , Label lblT, UnMOp MPush (Const 1), Label lblE] -- EAX > EBX
    where
        (Lbl lbl) = newLabel id
        lblT = (Lbl (lbl++"_true"))
        lblE = (Lbl (lbl++"_end"))

toSymbInstr SGte = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX)
                   , JmpMOp MJGE lblT, UnMOp MPush (Const 0), JmpMOp MJmp lblE
                   , Label lblT, UnMOp MPush (Const 1), Label lblE] -- EAX > EBX
    where
        (Lbl lbl) = newLabel id
        lblT = (Lbl (lbl++"_true"))
        lblE = (Lbl (lbl++"_end"))


toSymbInstr (SJEq  label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX), JmpMOp MJE  (Lbl label)]
toSymbInstr (SJNeq label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX), JmpMOp MJNE (Lbl label)]
toSymbInstr (SJLt  label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX), JmpMOp MJL  (Lbl label)]
toSymbInstr (SJLte label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX), JmpMOp MJLE (Lbl label)]
toSymbInstr (SJGt  label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX), JmpMOp MJG  (Lbl label)]
toSymbInstr (SJGte label) = [UnMOp MPop (Reg EAX), UnMOp MPop (Reg EBX), BinMOp MCmp (Reg EBX) (Reg EAX), JmpMOp MJGE (Lbl label)]


toSymbInstr SNot = [UnMOp MNot (DWord (Indirect (Reg ESP)))]
toSymbInstr SNeg = [UnMOp MNeg (DWord (Indirect (Reg ESP)))]
toSymbInstr SInc = [UnMOp MInc (DWord (Indirect (Reg ESP)))]
toSymbInstr SDec = [UnMOp MDec (DWord (Indirect (Reg ESP)))]

toSymbInstr SInput = [JmpMOp MCall (Lbl "input_int"), UnMOp MPush (Reg EAX)]

toSymbInstr (SPrintS s) = [BinMOp MMov (Reg ECX) (ConstName s), BinMOp MMov (Reg EDX) (ConstName $ lblStrLen s), JmpMOp MCall (Lbl "output_str")]
toSymbInstr SPrintI = [UnMOp MPop (Reg EAX), JmpMOp MCall (Lbl "output_int")]

toSymbInstr (SPushI i) = [UnMOp MPush (Const i)]
toSymbInstr (SPushN n) = [UnMOp MPush (DWord (Name n))]
toSymbInstr (SPop n)   = [UnMOp MPop  (DWord (Name n))]

toSymbInstr (SGet n) = [UnMOp MPop (Reg EAX), BinMOp MMov (Reg EBX) (Name n) -- Get the index and name
                     , BinMOp MMov (Reg EAX) (IndirectScale (Reg EBX) Four (Reg EAX)), UnMOp MPush (Reg EAX)] -- Get the value itself and push it

toSymbInstr (SPut n) = [UnMOp MPop (Reg EAX), BinMOp MMov (Reg EBX) (Name n), UnMOp MPop (Reg ECX) -- Get the value, name and index
                     , BinMOp MMov (IndirectScale (Reg EBX) Four (Reg EAX)) (Reg ECX)] -- Put the value

toSymbInstr (SJump label)  = [JmpMOp MJmp (Lbl label)]
toSymbInstr (SJTrue label) = [UnMOp MPop (Reg EAX), BinMOp MCmp (Reg EAX) (Const 0), JmpMOp MJNE (Lbl label)]
toSymbInstr (SJFalse label) = [UnMOp MPop (Reg EAX), BinMOp MCmp (Reg EAX) (Const 0), JmpMOp MJE (Lbl label)]
toSymbInstr (SCall label)  = [JmpMOp MCall (Lbl label), UnMOp MPush (Reg EAX)] -- Grab value off eax once the function point has been returned to
toSymbInstr (SEnter)       = [UnMOp MPop (Reg EBP)]
toSymbInstr (SRestEnter)   = [UnMOp MPush (Reg EBP)]
toSymbInstr (SPushEax)     = [UnMOp MPush (Reg EAX)]
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
