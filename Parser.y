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
    'and'       { TokAnd }
    'but'       { TokBut }
    'then'      { TokThen }
    'Alice found' { TokAliceFound }
    'was a'     { TokWasA }
    'became'    { TokBecame }
    'ate'       { TokAte }
    'drank'     { TokDrank }
    'number'    { TokNumber }
    'too'       { TokToo }
    '+'         { TokBinOp '+' }
    '*'         { TokBinOp '*' }
    '|'         { TokBinOp '|' }
    '^'         { TokBinOp '^' }
    '&'         { TokBinOp '&' }
    '/'         { TokBinOp '/' }
    '%'         { TokBinOp '%' }
    '~'         { TokUnOp '~'  }
    INT         { TokInt $$    }
    ID          { TokId $$     }

%%

Program     : Statements Output               { Program $1 $2 }

Statements  : Statements Statement Terminator { $1 ++ [$2] }
            | {- empty -}                     { [] }

Terminator  : ','                             {}
            | '.'                             {}
            | 'and'                           {}
            | 'but'                           {}
            | 'then'                          {}

Output      : 'Alice found' Exp '.'           { Result $2 }

Statement   : ID 'was a' Type Too             { Declare $1 $3 }
            | ID 'became' Exp                 { Assign $1 $3 }
            | ID 'ate'                        { Increment $1 }
            | ID 'drank'                      { Decrement $1 }

Type        : 'number'                        { Number }

Too         : 'too'                           {}
            | {- empty -}                     {}

Exp         : Exp '|' Exp1                    { Or $1 $3 }
            | Exp '^' Exp1                    { Xor $1 $3 }
            | Exp '&' Exp1                    { And $1 $3 }
            | Exp1                            { Exp1 $1 }

Exp1        : Exp1 '+' Exp2                   { Add $1 $3 }
            | Exp2                            { Exp2 $1 }

Exp2        : Exp2 '*' Exp3                   { Times $1 $3 }
            | Exp2 '/' Exp3                   { Div $1 $3 }
            | Exp2 '%' Exp3                   { Mod $1 $3 }
            | Exp3                            { Exp3 $1 }

Exp3        : '~' Exp3                        { Not $2 }
            | Val                             { Val $1 }

Val         : INT                             { Int $1 }
            | ID                              { Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

--main = getContents >>= print . parse . Lexer.alexScanTokens
}

