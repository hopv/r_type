# 1 "Quallexer.mll"
 
(* A sample input for ocamllex.
   Usage:
     1. Run "ocamllex lexer.mll"
     2. Invoke an ocaml interpreter; and run:
      #use "lexer.ml";;
      main <an input file name for the lexical analyzer>
 *)
(* This part will be attached to the beginning of the generated code *)
open Qualparser
open Lexing

let line_no = ref 1  (* the current line number, used for error reporting *)
let end_of_previousline = ref 0
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
(* data type declaration for tokens *)
(*
type token = EQ | NEQ | LEQ | LT | GEQ | GT | PLUS | MINUS | TIMES | LPAREN | RPAREN
           | LET | REC| IN | IF | THEN | ELSE | FUNC | INT of int | ID of string | EOF
 *)

# 31 "Quallexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\228\255\229\255\077\000\160\000\235\000\054\001\011\000\
    \001\000\236\255\064\001\002\000\030\000\003\000\246\255\247\255\
    \248\255\250\255\002\000\031\000\253\255\254\255\002\000\240\255\
    \251\255\245\255\243\255\244\255\241\255\139\001\214\001\235\255\
    \234\255\128\000\252\255\253\255\004\000\006\000\255\255\254\255\
    ";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\025\000\024\000\023\000\022\000\027\000\
    \027\000\255\255\025\000\016\000\013\000\017\000\255\255\255\255\
    \255\255\255\255\006\000\003\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\025\000\018\000\255\255\
    \255\255\255\255\255\255\255\255\003\000\003\000\255\255\255\255\
    ";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\000\000\
    \000\000\034\000\000\000\000\000\255\255\255\255\000\000\000\000\
    ";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\022\000\021\000\022\000\000\000\022\000\000\000\022\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \022\000\013\000\022\000\000\000\000\000\000\000\008\000\031\000\
    \018\000\016\000\017\000\020\000\024\000\019\000\039\000\038\000\
    \009\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\000\000\015\000\012\000\014\000\011\000\028\000\
    \025\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\004\000\003\000\
    \003\000\003\000\003\000\026\000\027\000\023\000\000\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\010\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\005\000\003\000\
    \003\000\003\000\003\000\000\000\007\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\032\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \036\000\000\000\037\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\003\000\
    \002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \035\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\029\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\030\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\022\000\255\255\000\000\255\255\022\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\022\000\255\255\255\255\255\255\000\000\008\000\
    \000\000\000\000\000\000\000\000\018\000\000\000\036\000\037\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\011\000\
    \013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\012\000\012\000\019\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\007\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \033\000\255\255\033\000\255\255\003\000\255\255\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\255\255\255\255\255\255\255\255\004\000\
    \000\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\255\255\255\255\
    \255\255\255\255\005\000\255\255\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \033\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\255\255\255\255\255\255\255\255\010\000\
    \255\255\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\255\255\255\255\
    \255\255\255\255\029\000\255\255\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\255\255\255\255\255\255\255\255\030\000\255\255\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 42 "Quallexer.mll"
            ( token lexbuf )
# 259 "Quallexer.ml"

  | 1 ->
# 44 "Quallexer.mll"
    ( next_line lexbuf;
      token lexbuf)
# 265 "Quallexer.ml"

  | 2 ->
# 46 "Quallexer.mll"
      (PLUS)
# 270 "Quallexer.ml"

  | 3 ->
# 47 "Quallexer.mll"
      (MINUS)
# 275 "Quallexer.ml"

  | 4 ->
# 48 "Quallexer.mll"
       ( comment lexbuf; token lexbuf )
# 280 "Quallexer.ml"

  | 5 ->
# 49 "Quallexer.mll"
      (TIMES)
# 285 "Quallexer.ml"

  | 6 ->
# 50 "Quallexer.mll"
      (LPAREN)
# 290 "Quallexer.ml"

  | 7 ->
# 51 "Quallexer.mll"
      (RPAREN)
# 295 "Quallexer.ml"

  | 8 ->
# 52 "Quallexer.mll"
      (SEMICOLON)
# 300 "Quallexer.ml"

  | 9 ->
# 53 "Quallexer.mll"
      (EQ)
# 305 "Quallexer.ml"

  | 10 ->
# 54 "Quallexer.mll"
       (NEQ)
# 310 "Quallexer.ml"

  | 11 ->
# 55 "Quallexer.mll"
       (NEQ)
# 315 "Quallexer.ml"

  | 12 ->
# 56 "Quallexer.mll"
       (LEQ)
# 320 "Quallexer.ml"

  | 13 ->
# 57 "Quallexer.mll"
      (LT)
# 325 "Quallexer.ml"

  | 14 ->
# 58 "Quallexer.mll"
       (GEQ)
# 330 "Quallexer.ml"

  | 15 ->
# 59 "Quallexer.mll"
       (IMPL)
# 335 "Quallexer.ml"

  | 16 ->
# 60 "Quallexer.mll"
      (GT)
# 340 "Quallexer.ml"

  | 17 ->
# 61 "Quallexer.mll"
      (NOT)
# 345 "Quallexer.ml"

  | 18 ->
# 62 "Quallexer.mll"
        (NOT)
# 350 "Quallexer.ml"

  | 19 ->
# 63 "Quallexer.mll"
      (INT(0))
# 355 "Quallexer.ml"

  | 20 ->
# 64 "Quallexer.mll"
       (AND)
# 360 "Quallexer.ml"

  | 21 ->
# 65 "Quallexer.mll"
       (OR)
# 365 "Quallexer.ml"

  | 22 ->
# 67 "Quallexer.mll"
   (let s = Lexing.lexeme lexbuf in INT(int_of_string s))
# 370 "Quallexer.ml"

  | 23 ->
# 70 "Quallexer.mll"
      ( NU )
# 375 "Quallexer.ml"

  | 24 ->
# 71 "Quallexer.mll"
      ( NU )
# 380 "Quallexer.ml"

  | 25 ->
# 73 "Quallexer.mll"
    ( let s = Lexing.lexeme lexbuf in ID(s))
# 385 "Quallexer.ml"

  | 26 ->
# 74 "Quallexer.mll"
      ( EOF )
# 390 "Quallexer.ml"

  | 27 ->
# 76 "Quallexer.mll"
    ( Format.eprintf "unknown token %s in line %d, column %d-%d @."
	(Lexing.lexeme lexbuf)
        (!line_no)
	((Lexing.lexeme_start lexbuf)- (!end_of_previousline))
	((Lexing.lexeme_end lexbuf)-(!end_of_previousline));
      failwith "lex error" )
# 400 "Quallexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 33
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 86 "Quallexer.mll"
    ( () )
# 412 "Quallexer.ml"

  | 1 ->
# 88 "Quallexer.mll"
    ( comment lexbuf; comment lexbuf )
# 417 "Quallexer.ml"

  | 2 ->
# 90 "Quallexer.mll"
    (  print_string "Lex error: unterminated comment\n";
       failwith "unterminated comment" )
# 423 "Quallexer.ml"

  | 3 ->
# 93 "Quallexer.mll"
    ( comment lexbuf )
# 428 "Quallexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;

# 95 "Quallexer.mll"
 
(* This part is added to the end of the generated code *)
(* The following is a piece of code for testing the generated lexical analyzer. *)
  (*
let rec readloop lexbuf =
  let t = token lexbuf in
    if t=EOF then []
    else t::(readloop lexbuf)
    *)

(* main takes a filename, performs a lexical analysis, and
   returns the result as a list of tokens.
 *)
let read filename = Lexing.from_channel (open_in filename)
let main filename = token (read filename)

# 452 "Quallexer.ml"