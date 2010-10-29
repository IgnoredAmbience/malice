module Main where
import Lexer
import Parser
main = getContents >>= print . parse . Lexer.alexScanTokens

