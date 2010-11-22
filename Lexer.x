{
module Lexer where
import Types
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
  $white+               ;
  \.                    { \s -> TokDot }
  \,                    { \s -> TokComma }
  \?                    { \s -> TokQuestion }
  and                   { \s -> TokAnd }
  but                   { \s -> TokBut }
  then                  { \s -> TokThen }
  too                   { \s -> TokToo }

  Alice                 { \s -> TokAlice }
  found                 { \s -> TokFound }
  was                   { \s -> TokWas }
  a                     { \s -> TokA }
  became                { \s -> TokBecame }
  ate                   { \s -> TokAte }
  drank                 { \s -> TokDrank }
  what                  { \s -> TokWhat }
  thought               { \s -> TokThought }
  said                  { \s -> TokSaid }
  spoke                 { \s -> TokSpoke }

  had                   { \s -> TokHad }
  \'s                   { \s -> TokArrS }
  piece                 { \s -> TokPiece }

  The                   { \s -> TokThe }
  room                  { \s -> TokRoom }
  contained             { \s -> TokContained }
  Looking-Glass         { \s -> TokLookingGlass }
  changed               { \s -> TokChanged }

  eventually            { \s -> TokEventually }
  because               { \s -> TokBecause }
  enough                { \s -> TokEnough }
  times                 { \s -> TokTimes }

  perhaps               { \s -> TokPerhaps }
  so                    { \s -> TokSo }
  or                    { \s -> TokOr }
  maybe                 { \s -> TokMaybe }
  unsure                { \s -> TokUnsure }
  which                 { \s -> TokWhich }

  number                { \s -> TokNumberType }
  letter                { \s -> TokLetterType }
  sentence              { \s -> TokSentenceType }

  $digit+               { \s -> TokInt (read s) }
  [\+\-\*\|\^\&\/\%\<\>] { \s -> TokBinOp (head s) }
  ==                    { \s -> TokBinOp '=' }
  \<=                   { \s -> TokBinOp 'l' }
  \>=                   { \s -> TokBinOp 'g' }
  !=                    { \s -> TokBinOp '!' }
  [\~]                  { \s -> TokUnOp (head s) }
  [\(]                  { \s -> TokLBrace }
  [\)]                  { \s -> TokRBrace }
  [$alpha\_]+           { \s -> TokId s }
  \'.\'                 { \s -> TokChar (s!!1) }
  \"[^\"]*\"            { \s -> TokStr s }

{
-- Each action has type :: String -> Token

-- The token type:

{-
main = do
  s <- getContents
  print (alexScanTokens s)
-}
}
