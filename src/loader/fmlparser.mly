%{
  open Objt
  open Cond
  open Type
  open Op
%}

%token EQ LEQ GEQ GT LT NEQ
%token AND OR
%token NOT

%token <int> INT
%token <Objt.id> ID
%token MINUS PLUS TIMES DIV MOD
%token EOF LPAREN RPAREN LBRACE RBRACE COLON VLINE IF THEN  ELSE  LET REC FUNC INTEGER IN FAIL ASSERT RECAND SELECT WHEN ARROW TRUE FALSE IMPL

%left ASSERT

%left AND OR IMPL

%nonassoc EQ LEQ GEQ GT LT NEQ

%left PLUS MINUS

%left TIMES DIV

%nonassoc UMINUS
%right NOT

%start main
%type <Type.Env.t> main

%%

main:
  | typedef EOF { Type.Env.T.(Type.Env.empty @<< from_map $1) }
  | typedef main { Type.Env.T.($2 @<< from_map $1) }

typedef:
  | ID COLON ref_type { ($1, $3) }

ref_type:
  | LPAREN ref_type RPAREN                     { $2 }
  | LBRACE ID COLON INTEGER VLINE condition RBRACE { RefType.Int_ ($2, $6)}
  | func_type           { $1 }

func_type:
  | LPAREN func_type RPAREN          { $2 }
  | ID COLON ref_type ARROW ref_type { RefType.Func ($1, $3, $5) }
  | ref_type ARROW ref_type          { RefType.Func (RefType.L.gen (), $1, $3) }

condition:
  | LPAREN condition RPAREN       { $2 }
  | condition PLUS  condition     { Op2($1, Plus , $3) }
  | condition MINUS condition     { Op2($1, Minus, $3) }
  | condition MOD condition       { Op2($1, Mod, $3)   }
  | condition TIMES condition     { Op2($1, Times, $3) }
  | condition DIV   condition     { Op2($1, Div  , $3) }
  | condition EQ    condition     { Op2($1, Eq   , $3) }
  | condition NEQ   condition     { Op2($1, Neq  , $3) }
  | condition LEQ   condition     { Op2($1, Leq  , $3) }
  | condition LT    condition     { Op2($1, Lt   , $3) }
  | condition GEQ   condition     { Op2($1, Geq  , $3) }
  | condition GT    condition     { Op2($1, Gt   , $3) }
  | condition AND condition       { Op2($1, And_, $3)  }
  | condition OR condition        { Op2($1, Or_, $3)   }
  | condition IMPL condition      { Op2($1, Impl, $3)  }
  | MINUS condition               { Op1(Minus, $2) }
  | NOT condition                 { Op1(Not_, $2) }
  | value                         { Value($1) }
;

value:
  | ID                 { VarObj($1) }
  | INT                { IntObj($1) }
  | TRUE               { IntObj(1) }
  | FALSE              { IntObj(0) }
;
