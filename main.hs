module Main where
import Lexer
import Parser
import Semantics
import Translator
import Output
import AbstractOutput
import Peephole
import TranslatorOptimize
import System (getArgs)
import System.Console.GetOpt -- http://leiffrenzel.de/papers/commandline-options-in-haskell.html 
import Text.Groom

main :: IO()
main = do
  args <- getArgs
--  let (flags,unknownFlags,errorMessages) = getOpt RequireOrder options args
  case getOpt RequireOrder options args of
    (f, [] , [] )     -> processFlags f
    (_, unknowns, []) -> (useUnknowns unknowns)
    (_,_,errorMsgs)   -> error $ concat errorMsgs

data Flag = Output String | Input String | OutputMade OutputStage
  deriving (Show)
data OutputStage = Lexer | Parser | Semantics | Translator | Optimise | Assembler
  deriving (Show, Eq,Ord)

options :: [OptDescr Flag]
options = [ Option ['S','s'] ["semantics"]           (NoArg (OutputMade Semantics))  "output the symbol table", 
            Option ['L','l'] ["lexer"]               (NoArg (OutputMade Lexer))      "output the token list",
            Option ['P','p'] ["parser"]              (NoArg (OutputMade Parser))     "output the AST",
            Option ['T','t'] ["translate"]           (NoArg (OutputMade Translator)) "output intermediary assembly",
            Option ['A','a'] ["assembly"]            (NoArg (OutputMade Assembler))  "output the generated assembly",
            Option ['O','o'] ["optimise","optimize"] (NoArg (OutputMade Optimise))   "some peephole optimisations",
            Option ['F','f'] ["in","input","file"] (ReqArg Input "FILE")   "input from FILE"
          ]
useUnknowns :: [String] -> IO ()
useUnknowns (x:_) = readFile x >>= putStrLn.assemble
useUnknowns []     = return ()

               
assemble :: String -> String
assemble source =  unlines {-. concat-} . output dataTbl symbolTables . abstract . translate  $ program
  where
    ((program, symbolTables), dataTbl) = semantics . parse $ alexScanTokens source

optimise :: String -> String
optimise source =  unlines . output dataTbl symbolTables . peephole . abstract . transOptimize . translate  $ program
  where
    ((program, symbolTables), dataTbl) = semantics . parse $ alexScanTokens source

processFlags :: [Flag] -> IO () -- empty string is stdin / stdout respectively
processFlags fs = input >>= 
                  case compileStage of
                      Lexer      -> outputData . groomString . show . alexScanTokens 
                      Parser     -> outputData . groomString . show . parse . alexScanTokens
                      Semantics  -> outputData . groomString . show . semantics . parse . alexScanTokens
                      Translator -> outputData . groomString . show . translate . fst . fst . semantics . parse . alexScanTokens
                      Optimise   -> outputData . optimise
                      Assembler  -> outputData . assemble
  where
    (compileStage, inFrom, outTo) = foldr processFlagStep (Assembler, "", "") fs

    input = case inFrom of
              "" -> getContents
              xs -> readFile xs

    outputData = case outTo of
            "" -> putStrLn
            xs -> writeFile xs

    
    processFlagStep :: Flag -> (OutputStage, String, String) -> (OutputStage, String, String) 
    processFlagStep f (s, i, o) = case f of
                                    OutputMade s' -> (min s s', i, o)
                                    Input inFile   -> (s, inFile, o)
                                    Output outFile -> (s, i, outFile)

