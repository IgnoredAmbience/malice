{
module Lexer where
import Types
}

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
  spider                { \s -> TokSpider }
  "Looking-Glass"       { \s -> TokLookingGlass }
  changed               { \s -> TokChanged }
  went                  { \s -> TokWent }
  through               { \s -> TokThrough }

  eventually            { \s -> TokEventually }
  because               { \s -> TokBecause }
  enough$white+times    { \s -> TokEnoughTimes }

  perhaps               { \s -> TokPerhaps }
  either                { \s -> TokEither }
  so                    { \s -> TokSo }
  or                    { \s -> TokOr }
  maybe                 { \s -> TokMaybe }
  unsure                { \s -> TokUnsure }
  which                 { \s -> TokWhich }

  number                { \s -> TokNumberType }
  letter                { \s -> TokLetterType }
  sentence              { \s -> TokSentenceType }

  $digit+               { \s -> TokInt (read s) }
  [\+\-\*\|\^\&\/\%\<\>] { \s -> TokOp (head s) }
  "||"                  { \s -> TokOp 'o' }
  &&                    { \s -> TokOp 'a' }
  ==                    { \s -> TokOp '=' }
  \<=                   { \s -> TokOp 'l' }
  \>=                   { \s -> TokOp 'g' }
  !=                    { \s -> TokOp '!' }
  [\~]                  { \s -> TokOp (head s) }
  [\(]                  { \s -> TokLBrace }
  [\)]                  { \s -> TokRBrace }
  [$alpha\_][$alpha\_$digit]* { \s -> TokId s }
  \'.\'                 { \s -> TokChar (s!!1) }
  \"[^\"]*\"            { \s -> TokStr s }

{
-- Taken from the posn wrapper, since AlexPosn conflicts with definition in Types
type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  String)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,c,[]) = Nothing
alexGetChar (p,_,(c:s))  = let p' = alexMove p c in p' `seq`
                                Just (c, (p', c, s))
alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

--alexScanTokens :: String -> [(token, AlexPosn)]
alexScanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error $ "lexical error on line " ++ show (line pos) ++ " at column " ++ show (lineIdx pos)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> (act (take len str), pos) : go inp'
}
