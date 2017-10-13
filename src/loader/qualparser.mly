%{
  open Data.Objt
  open Data.Cond
  open Data.Type
  open Data.Op
%}

%token IMPL
%token AND OR
%token NOT
%token EQ LEQ GEQ GT LT NEQ

%token <int> INT
%token <string> ID
%token MINUS PLUS TIMES DIV
%token EOF LPAREN RPAREN TRUE FALSE NU SEMICOLON

%right IMPL
%left AND OR

%nonassoc EQ LEQ GEQ GT LT NEQ

%left PLUS MINUS

%left TIMES DIV

%right NOT

%start main
%type <Data.Cond.t list> main

%%

main:
  | EOF { [] }
  | condition EOF { [$1] }
  | condition SEMICOLON main { $1 :: $3 }

condition:
  | LPAREN condition RPAREN       { $2 }
  | condition PLUS  condition     { Op2($1, Plus , $3) }
  | condition MINUS condition     { Op2($1, Minus, $3) }
  | condition TIMES condition     { Op2($1, Times, $3) }
  | condition DIV   condition     { Op2($1, Div  , $3) }
  | condition EQ    condition     { Op2($1, Eq   , $3) }
  | condition NEQ   condition     { Op2($1, Neq  , $3) }
  | condition LEQ   condition     { Op2($1, Leq  , $3) }
  | condition LT    condition     { Op2($1, Lt   , $3) }
  | condition GEQ   condition     { Op2($1, Geq  , $3) }
  | condition GT    condition     { Op2($1, Gt   , $3) }
  | condition AND condition       { Op2($1, And_, $3) }
  | condition OR condition        { Op2($1, Or_, $3) }
  | condition IMPL condition     { Op2($1, Impl, $3) }
  | MINUS condition               { Op1(Minus, $2) }
  | NOT condition                 { Op1(Not_, $2) }
  | value                         { Value($1) }
;

value:
  | NU                 { VarObj("V") }
  | ID                 { VarObj($1) }
  | INT                { IntObj($1) }
  | TRUE               { IntObj(1) }
  | FALSE              { IntObj(0) }
;
