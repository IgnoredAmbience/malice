module Main where
import Lexer
import Parser
import Semantics
import Translator
import Text.Groom
main = getContents >>= putStrLn . groom . translate . parse . alexScanTokens
