
(* The type of tokens. *)

type token = 
  | UNSAT
  | SAT
  | OPAREN
  | OP of (string)
  | MODEL
  | LET
  | INT
  | IDENT of (string)
  | EXISTS
  | ERROR
  | EOF
  | DQUOTED of (string)
  | DEFINE
  | CPAREN
  | CINT of (string)
  | CBOOL of (string)
  | BOOL

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ParseBase.parse)
