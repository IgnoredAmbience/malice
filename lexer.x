{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
  $white+       ;
  \.                    { \s -> Dot }
  \,                    { \s -> Comma }
  and                   { \s -> And }
  but                   { \s -> But }
  then                  { \s -> Then }
  "Alice found"         { \s -> AliceFound }
  "was a"               { \s -> WasA }
  became                { \s -> Became }
  ate                   { \s -> Ate }
  drank                 { \s -> Drank }
  number                { \s -> Number }
  too                   { \s -> Too }
  $digit+               { \s -> Int (read s) }
  [\+\*\|\^\&\/\%]      { \s -> BinOp (head s) }
  [\~]                  { \s -> UnOp (head s) }
  [$alpha\_]+           { \s -> Id s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = Dot
           | Comma
           | And
           | But
           | Then
           | AliceFound
           | WasA
           | Became
           | Ate
           | Drank
           | Number
           | Too
           | UnOp Char
           | BinOp Char
           | Int Int
           | Id String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
