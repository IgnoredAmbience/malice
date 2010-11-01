module Main where
import Lexer
import Parser
import Text.Groom
main = getContents >>= putStrLn . groom . parse . alexScanTokens

