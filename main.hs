module Main where
import Lexer
import Parser
import Semantics
import Translator
import Text.Groom
import System.Environment

-- TODO, tidy up this mo'fo. check http://leiffrenzel.de/papers/commandline-options-in-haskell.html for info about better command line arg stuff

main :: IO()
main = do
  args <- getArgs
  case args of
    "-l":_      -> getContents   >>= putStrLn.groom.alexScanTokens
    "-p":_      -> getContents   >>= putStrLn.groom.parse.alexScanTokens 
    "-t":_      -> getContents   >>= putStrLn.unlines.map groom.translate.parse.alexScanTokens
    "-f":name:_ -> readFile name >>= putStrLn.unlines.map groom.translate.parse.alexScanTokens
    "-f":_      -> error "File name missing"  
    _           -> error "Please pipe the contents of an alice file in"



              
