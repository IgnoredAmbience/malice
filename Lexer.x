{
module Lexer where
import Types
}

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
  $white+               ;
  \.                    { \p s -> TokDot p }
  \,                    { \p s -> TokComma p }
  \?                    { \p s -> TokQuestion p }
  and                   { \p s -> TokAnd p }
  but                   { \p s -> TokBut p }
  then                  { \p s -> TokThen p }
  too                   { \p s -> TokToo p }

  Alice                 { \p s -> TokAlice p }
  found                 { \p s -> TokFound p }
  was                   { \p s -> TokWas p }
  a                     { \p s -> TokA p }
  became                { \p s -> TokBecame p }
  ate                   { \p s -> TokAte p }
  drank                 { \p s -> TokDrank p }
  what                  { \p s -> TokWhat p }
  thought               { \p s -> TokThought p }
  said                  { \p s -> TokSaid p }
  spoke                 { \p s -> TokSpoke p }

  had                   { \p s -> TokHad p }
  \'s                   { \p s -> TokArrS p }
  piece                 { \p s -> TokPiece p }

  The                   { \p s -> TokThe p }
  room                  { \p s -> TokRoom p }
  contained             { \p s -> TokContained p }
  "Looking-Glass"       { \p s -> TokLookingGlass p }
  changed               { \p s -> TokChanged p }
  went                  { \p s -> TokWent p }
  through               { \p s -> TokThrough p }

  eventually            { \p s -> TokEventually p }
  because               { \p s -> TokBecause p }
  enough$white+times    { \p s -> TokEnoughTimes p }

  perhaps               { \p s -> TokPerhaps p }
  either                { \p s -> TokEither p }
  so                    { \p s -> TokSo p }
  or                    { \p s -> TokOr p }
  maybe                 { \p s -> TokMaybe p }
  unsure                { \p s -> TokUnsure p }
  which                 { \p s -> TokWhich p }

  number                { \p s -> TokNumberType p }
  letter                { \p s -> TokLetterType p }
  sentence              { \p s -> TokSentenceType p }

  $digit+               { \p s -> TokInt p (read s) }
  [\+\-\*\|\^\&\/\%\<\>] { \p s -> TokOp p (head s) }
  "||"                  { \p s -> TokOp p 'o' }
  &&                    { \p s -> TokOp p 'a' }
  ==                    { \p s -> TokOp p '=' }
  \<=                   { \p s -> TokOp p 'l' }
  \>=                   { \p s -> TokOp p 'g' }
  !=                    { \p s -> TokOp p '!' }
  [\~]                  { \p s -> TokOp p (head s) }
  [\(]                  { \p s -> TokLBrace p }
  [\)]                  { \p s -> TokRBrace p }
  [$alpha\_]+           { \p s -> TokId p s }
  \'.\'                 { \p s -> TokChar p (s!!1) }
  \"[^\"]*\"            { \p s -> TokStr p s }

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

--alexScanTokens :: String -> [token]
alexScanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error $ "lexical error on line " ++ show (alexPosnLine pos) ++ " at column " ++ show (alexPosnChar pos)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'
}
