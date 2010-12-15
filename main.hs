module Main where
import Lexer
import Parser
import Semantics
import Translator
import Output
import System (getArgs)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt -- http://leiffrenzel.de/papers/commandline-options-in-haskell.html 
import Control.Monad (liftM)
import Control.Applicative ((<$>),(<*>))

main :: IO()
main = do
  args <- getArgs
  {- let (flags,unknownFlags,errorMessages) = getOpt RequireOrder options args
  case getOpt RequireOrder options args of
    (f, [] , [] )     -> processFlags f
    (_, unknowns, []) -> error $ "Did not recognise " ++ unwords unknowns
    (_,_,errorMsgs)      -> error $ concat errorMsgs
   -}
  case args of
    "-f":name:_ -> readFile name >>= compile
    "-f":_      -> error "File name missing"  
    _           -> getContents   >>= compile
    

data Flag = Output String | Input String | OutputMade OutputStage
data OutputStage = Lexer | Parser | Semantics | Assembly | Compile
  deriving (Eq,Ord)

options :: [OptDescr Flag]
options = [ Option ['S','s'] ["semantics"] (NoArg (OutputMade Semantics)) "output the symbol table",
            Option ['L','l'] ["lexer"]     (NoArg (OutputMade Lexer)) "output the token list",
            Option ['P','p'] ["parser"]    (NoArg (OutputMade Parser)) "output the AST",
            Option ['A','a'] ["assembly"]  (NoArg (OutputMade Assembly)) "output the generated assembly",                  
            Option ['O','o'] ["out","output"]      (OptArg makeOut "FILE") "output to FILE",
            Option ['F','f'] ["in","input","file"] (OptArg input "FILE")   "input from FILE"
          ]

makeOut :: Maybe String -> Flag
makeOut = Output . fromMaybe ""

input :: Maybe String -> Flag
input = Input . fromMaybe ""


compile :: String -> IO ()
compile source =  putStrLn . unlines . concat . output symbolTables . translate  $ program
  where
    (program, symbolTables) = semantics . parse $ alexScanTokens source


processFlags :: [Flag] -> IO () -- empty string is stdin / stdout respectively
processFlags fs = undefined
  where
    (compileStage, inFrom, outTo) = foldr processFlagStep (Compile, "", "") fs

    input = case inFrom of
              "" -> getContents
              xs -> readFile xs

    out = case outTo of
            "" -> putStrLn
            xs -> writeFile xs

    symTab  =  fst <$> semantics <$> program
    program = parse . alexScanTokens <$> input

    compileAction = case compileStage of
                      Lexer     -> input >>= putStrLn . show . alexScanTokens 
                      Parser    -> input >>= putStrLn . show . parse . alexScanTokens
                      Semantics -> input >>= putStrLn . show . fst . semantics . parse . alexScanTokens
                      -- Assembly  -> input >>= putStrLn . unlines . output symTab . translate . parse . alexScanTokens
                      Compile   -> undefined

    processFlagStep :: Flag -> (OutputStage, String, String) -> (OutputStage, String, String) 
    processFlagStep f (s, i, o) = case f of
                                    OutputMade s' -> (min s s', i, o)
                                    Input inFile   -> (s, inFile, o)
                                    Output outFile -> (s, i, outFile)

