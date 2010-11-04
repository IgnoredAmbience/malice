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
    'Alice'     { TokAlice }
    'found'     { TokFound }
    'was'       { TokWas }
    'a'         { TokA }
    'became'    { TokBecame }
    'ate'       { TokAte }
    'drank'     { TokDrank }
    'number'    { TokNumberType }
    'letter'    { TokLetterType }
    'too'       { TokToo }
    '+'         { TokBinOp '+' }
    '-'         { TokBinOp '-' }
    '*'         { TokBinOp '*' }
    '|'         { TokBinOp '|' }
    '^'         { TokBinOp '^' }
    '&'         { TokBinOp '&' }
    '/'         { TokBinOp '/' }
    '%'         { TokBinOp '%' }
    '~'         { TokUnOp '~'  }
    INT         { TokInt $$    }
    ID          { TokId $$     }
    CHAR        { TokChar $$   }

%%

Program     : Statements Output               { Program $1 $2 }

Statements  : Statements Statement Terminator { $1 ++ [$2] }
            | {- empty -}                     { [] }

Terminator  : ','                             {}
            | '.'                             {}
            | 'and'                           {}
            | 'but'                           {}
            | 'then'                          {}

Output      : 'Alice' 'found' Exp '.'         { $3 }

Statement   : ID 'was' 'a' Type Too           { Declare $1 $4 }
            | ID 'became' Exp                 { Assign $1 $3 }
            | ID 'ate'                        { Increment $1 }
            | ID 'drank'                      { Decrement $1 }

Type        : 'number'                        { Number }
            | 'letter'                        { Letter }

Too         : 'too'                           {}
            | {- empty -}                     {}

Exp         : Exp '|' Exp1                    { BinOp Or $1 $3 }
            | Exp1                            { $1  }

Exp1        : Exp1 '^' Exp2                   { BinOp Xor $1 $3 }
            | Exp2                            { $1  }

Exp2        : Exp2 '&' Exp3                   { BinOp And $1 $3 }
            | Exp3                            { $1  }

Exp3        : Exp3 '+' Exp4                   { BinOp Add $1 $3 }
            | Exp3 '-' Exp4                   { BinOp Subtract $1 $3 }
            | Exp4                            { $1 }

Exp4        : Exp4 '*' Exp5                   { BinOp Times $1 $3 }
            | Exp4 '/' Exp5                   { BinOp Div $1 $3 }
            | Exp4 '%' Exp5                   { BinOp Mod $1 $3 }
            | Exp5                            { $1 }

Exp5        : '~' Exp5                        { UnOp Not $2 }
            | Val                             { $1 }

Val         : INT                             { Int $1 }
            | ID                              { Var $1 }
            | CHAR                            { Char $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

--main = getContents >>= print . parse . Lexer.alexScanTokens
}

