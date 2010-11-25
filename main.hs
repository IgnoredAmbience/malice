module Main where
import Lexer
import Parser
import Semantics
import Translator
import Output
import System (getArgs)
import System.Console.GetOpt -- http://leiffrenzel.de/papers/commandline-options-in-haskell.html 

main :: IO()
main = do
  args <- getArgs
  let (_,_,_) = getOpt RequireOrder options args
  case args of
    "-f":name:_ -> readFile name >>= compile
    "-f":_      -> error "File name missing"  
    _           -> getContents   >>= compile

data Flag = Semantics | Lexer | Assembler | Parser

options :: [OptDescr Flag]
options = [ Option ['S'] ["semantics"] (NoArg Semantics) "output the symbol  table",
            Option ['L'] ["lexer"]     (NoArg Lexer)     "output the token list",
            Option ['P'] ["parser"]    (NoArg Parser)    "output the AST",
            Option ['A'] ["assembly"]  (NoArg Assembler) "output the assembler code"                        
          ]









compile :: String -> IO ()
compile source = putStrLn . unlines . output st $ translate program
  where
    (st, _) = semantics program
    program = parse $ alexScanTokens source


