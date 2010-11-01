module Main where
import Lexer
import Parser
import Semantics
import Text.Groom
main = getContents >>= putStrLn . groom . semantics . parse . alexScanTokens

