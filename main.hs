module Main where
import Lexer
import Parser
import Semantics
import Translator
import Output
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
data OutputStage = Lexer | Parser | Semantics | Assembly | Compile
  deriving (Show, Eq,Ord)

options :: [OptDescr Flag]
options = [ Option ['S','s'] ["semantics"] (NoArg (OutputMade Semantics)) "output the symbol table",
            Option ['L','l'] ["lexer"]     (NoArg (OutputMade Lexer)) "output the token list",
            Option ['P','p'] ["parser"]    (NoArg (OutputMade Parser)) "output the AST",
            Option ['A','a'] ["assembly"]  (NoArg (OutputMade Compile)) "output the generated assembly",                  
            Option ['O','o'] ["out","output"]      (ReqArg Output "FILE") "output to FILE",
            Option ['F','f'] ["in","input","file"] (ReqArg Input "FILE")   "input from FILE"
          ]
useUnknowns :: [String] -> IO ()
useUnknowns (x:_) = readFile x >>= putStrLn.assemble
useUnknowns []     = return ()

               
assemble :: String -> String
assemble source =  unlines . concat . output symbolTables . translate  $ program
  where
    (program, symbolTables) = semantics . parse $ alexScanTokens source

processFlags :: [Flag] -> IO () -- empty string is stdin / stdout respectively
processFlags fs = input >>= 
                  case compileStage of
                      Lexer     -> outputData . groomString . show . alexScanTokens 
                      Parser    -> outputData . groomString . show . parse . alexScanTokens
                      Semantics -> outputData . groomString . show . snd . semantics . parse . alexScanTokens
                      Compile   -> outputData . assemble
  where
    (compileStage, inFrom, outTo) = foldr processFlagStep (Compile, "", "") fs

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
