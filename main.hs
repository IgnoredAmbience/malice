module Main where
import Lexer
import Parser
import Semantics
import Translator
import Output
import Text.Groom
import System.Environment

-- TODO, tidy up this mo'fo. check http://leiffrenzel.de/papers/commandline-options-in-haskell.html for info about better command line arg stuff

main :: IO()
main = do
  args <- getArgs
  case args of
    "-l":_      -> getContents   >>= putStrLn . groom . alexScanTokens
    "-p":_      -> getContents   >>= putStrLn . groom . parse . alexScanTokens 
    "-a":_      -> getContents   >>= putStrLn . unlines . map groom . translate . parse . alexScanTokens
    "-f":name:_ -> readFile name >>= compile
    "-f":_      -> error "File name missing"  
    _           -> getContents   >>= compile

compile :: String -> IO ()
compile source = putStrLn . unlines . output st $ translate program
  where
    (st, _) = semantics program
    program = parse $ alexScanTokens source
