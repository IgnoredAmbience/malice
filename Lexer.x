{
module Lexer where
import Types
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
  $white+       ;
  \.                    { \s -> TokDot }
  \,                    { \s -> TokComma }
  and                   { \s -> TokAnd }
  but                   { \s -> TokBut }
  then                  { \s -> TokThen }
  Alice                 { \s -> TokAlice }
  found                 { \s -> TokFound }
  was                   { \s -> TokWas }
  a                     { \s -> TokA }
  became                { \s -> TokBecame }
  ate                   { \s -> TokAte }
  drank                 { \s -> TokDrank }
  number                { \s -> TokNumberType }
  letter                { \s -> TokLetterType }
  too                   { \s -> TokToo }
  $digit+               { \s -> TokInt (read s) }
  [\+\-\*\|\^\&\/\%]    { \s -> TokBinOp (head s) }
  [\~]                  { \s -> TokUnOp (head s) }
  [$alpha\_]+           { \s -> TokId s }
  \'.\'                 { \s -> TokChar (s!!1) }

{
-- Each action has type :: String -> Token

-- The token type:

{-
main = do
  s <- getContents
  print (alexScanTokens s)
-}
}
