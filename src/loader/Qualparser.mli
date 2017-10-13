type token =
  | IMPL
  | AND
  | OR
  | NOT
  | EQ
  | LEQ
  | GEQ
  | GT
  | LT
  | NEQ
  | INT of (int)
  | ID of (string)
  | MINUS
  | PLUS
  | TIMES
  | DIV
  | EOF
  | LPAREN
  | RPAREN
  | TRUE
  | FALSE
  | NU
  | SEMICOLON

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Data.Cond.t list
