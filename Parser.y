{
module Parser where
import Types
}
%name parse 
%tokentype { Token }
%error { parseError }

%token
    '.'         { TokDot }
    ','         { TokComma }
    '?'         { TokQuestion }
    'and'       { TokAnd }
    'but'       { TokBut }
    'then'      { TokThen }
    'too'       { TokToo }

    'Alice'     { TokAlice }
    'found'     { TokFound }
    'was'       { TokWas }
    'a'         { TokA }
    'became'    { TokBecame }
    'ate'       { TokAte }
    'drank'     { TokDrank }
    'what'      { TokWhat }
    'thought'   { TokThought }
    'said'      { TokSaid }
    'spoke'     { TokSpoke }

    'had'       { TokHad }
    's'         { TokArrS }
    'piece'     { TokPiece }

    'The'       { TokThe }
    'room'      { TokRoom }
    'contained' { TokContained }
    'Looking-Glass' { TokLookingGlass }
    'changed'   { TokChanged }
    'went'      { TokWent }
    'through'   { TokThrough }

    'eventually' { TokEventually }
    'because'   { TokBecause }
    'enough times'    { TokEnoughTimes }

    'perhaps'   { TokPerhaps }
    'either'    { TokEither }
    'so'        { TokSo }
    'or'        { TokOr }
    'maybe'     { TokMaybe }
    'unsure'    { TokUnsure }
    'which'     { TokWhich }

    'number'    { TokNumberType }
    'letter'    { TokLetterType }
    'sentence'  { TokSentenceType }

    '+'         { TokOp '+' }
    '-'         { TokOp '-' }
    '*'         { TokOp '*' }
    '|'         { TokOp '|' }
    '^'         { TokOp '^' }
    '&'         { TokOp '&' }
    '/'         { TokOp '/' }
    '%'         { TokOp '%' }
    '~'         { TokOp '~' }
    '<'         { TokOp '<' }
    '>'         { TokOp '>' }
    '=='        { TokOp '=' }
    '<='        { TokOp 'l' }
    '>='        { TokOp 'g' }
    '!='        { TokOp '!' }
    '||'        { TokOp 'o' }
    '&&'        { TokOp 'a' }
    '('         { TokLBrace }
    ')'         { TokRBrace }
    INT         { TokInt $$  }
    ID          { TokId $$   }
    CHAR        { TokChar $$ }
    STR         { TokStr $$  }

%%

Program     : Statements Functions            { Program $1 $2 }

Statements  : Statements Statement Terminator { $1 ++ [$2] }
            | {- empty -}                     { [] }

Terminator  : ','                             {}
            | '.'                             {}
            | '?'                             {}
            | 'and'                           {}
            | 'but'                           {}
            | 'then'                          {}

Functions   : Functions Function              { $1 ++ [$2] }
            | Functions Lambda                { $1 ++ [$2] }
            | {- empty -}                     { [] }

Function    : 'The' 'room' ID '(' ParameterDefs ')' 'contained' 'a' Type Statements { Function $3 $9 $5 $10 }

ParameterDefs : ParameterDefs ',' ParameterDef { $1 ++ [$3] }
              | ParameterDef                  { [$1] }
              | {- empty -}                   { [] }

ParameterDef : Type ID                        { ($1, $2) }

Lambda      : 'The' 'Looking-Glass' ID 'changed' 'a' Type Statements { Lambda $3 $6 $7 }

Statement   : ID 'was' 'a' Type Too           { Declare $1 $4 }
            | ID 'had' Exp Type               { DeclareArr $1 $4 $3 }
            | Variable 'became' Exp           { Assign $1 $3 }
            | Variable 'ate'                  { Increment $1 }
            | Variable 'drank'                { Decrement $1 }
            | Variable 'went' 'through' ID    { LambdaApply $4 $1 }
            | 'what' 'was' Variable           { Input $3 }
            | STR 'thought' 'Alice'           { Comment $1 }
            | Exp 'said' 'Alice'              { Output $1 }
            | Exp 'spoke'                     { Output $1 }
            | 'Alice' 'found' Exp             { Return $3 }
            | 'eventually' '(' Exp ')' 'because' Statements 'enough times' { LoopUntil $3 $6 }
            | If '(' Exp ')' 'so' Statements ElseIf { If $3 $6 $7 }

If          : 'perhaps'                       {}
            | 'either'                        {}

ElseIf      : 'or' 'maybe' '(' Exp ')' 'so' Statements ElseIf { [If $4 $7 $8] }
            | 'or' Statements EndIf           { $2 }
            | EndIf                           { [] }

EndIf       : 'Alice' 'was' 'unsure' Which  {}
            
Which       : 'which'                         {}
            | {- empty -}                     {}

Type        : 'number'                        { Number }
            | 'letter'                        { Letter }
            | 'sentence'                      { Sentence }

Too         : 'too'                           {}
            | {- empty -}                     {}

Exp         : Exp '||' Exp1                   { BinOp LOr $1 $3 }
            | Exp1                            { $1 }

Exp1        : Exp1 '&&' Exp2                  { BinOp LAnd $1 $3 }
            | Exp2                            { $1 }

Exp2        : Exp2 '|' Exp3                   { BinOp Or $1 $3 }
            | Exp3                            { $1 }

Exp3        : Exp3 '^' Exp4                   { BinOp Xor $1 $3 }
            | Exp4                            { $1 }

Exp4        : Exp4 '&' Exp5                   { BinOp And $1 $3 }
            | Exp5                            { $1 }

Exp5        : Exp5 '==' Exp6                  { BinOp Eq $1 $3 }
            | Exp5 '!=' Exp6                  { BinOp Neq $1 $3 }
            | Exp6                            { $1 }

Exp6        : Exp6 '<' Exp7                   { BinOp Lt $1 $3 }
            | Exp6 '<=' Exp7                  { BinOp Lte $1 $3 }
            | Exp6 '>' Exp7                   { BinOp Gt $1 $3 }
            | Exp6 '>=' Exp7                  { BinOp Gte $1 $3 }
            | Exp7                            { $1 }

Exp7        : Exp7 '+' Exp8                   { BinOp Add $1 $3 }
            | Exp7 '-' Exp8                   { BinOp Sub $1 $3 }
            | Exp8                            { $1 }

Exp8        : Exp8 '*' Exp9                   { BinOp Mul $1 $3 }
            | Exp8 '/' Exp9                   { BinOp Div $1 $3 }
            | Exp8 '%' Exp9                   { BinOp Mod $1 $3 }
            | Exp9                            { $1 }

Exp9        : '~' Exp9                        { UnOp Not $2 }
            | '-' Exp9                        { UnOp Neg $2 }
            | Val                             { $1 }

Val         : INT                             { Int $1 }
            | CHAR                            { Char $1 }
            | STR                             { Str $1 }
            | Variable                        { Variable $1 }
            | ID '(' Parameters ')'           { FunctionCall $1 $3 }

Variable    : ID                              { Var $1 }
            | ID 's' Exp 'piece'              { VarArr $1 $3 } 

Parameters  : Parameters Terminator Exp       { $1 ++ [$3] }
            | Exp                             { [$1] }
            | {- empty -}                     { [] }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

--main = getContents >>= print . parse . Lexer.alexScanTokens
}

