
(* The type of tokens. *)

type token = 
  | WHEN
  | VLINE
  | TRUE
  | TIMES
  | THEN
  | SELECT
  | RPAREN
  | RECAND
  | REC
  | RBRACE
  | PLUS
  | OR
  | NOT
  | NEQ
  | MINUS
  | LT
  | LPAREN
  | LET
  | LEQ
  | LBRACE
  | INTEGER
  | INT of (int)
  | IN
  | IMPL
  | IF
  | ID of (Objt.id)
  | GT
  | GEQ
  | FUNC
  | FALSE
  | FAIL
  | EQ
  | EOF
  | ELSE
  | DIV
  | COLON
  | ASSERT
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Type.Env.t)
