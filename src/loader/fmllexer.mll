{
(* A sample input for ocamllex.
   Usage:
     1. Run "ocamllex lexer.mll"
     2. Invoke an ocaml interpreter; and run:
      #use "lexer.ml";;
      main <an input file name for the lexical analyzer>
 *)
(* This part will be attached to the beginning of the generated code *)
open Fmlparser
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
}

(* abbreviations *)
let space = [' ' '\t' '\r']
let newline = ['\n']
let digit = ['0'-'9']
let digitnz = ['1'-'9']
let mark = ['_']
let lower = ['a'-'z']
let higher = ['A'-'Z']
let minus = ['-']

(* The main part, defining tokens and the corresponding actions *)
rule token = parse
| space+    { token lexbuf }
| newline
    { next_line lexbuf;
      token lexbuf}
| "+" {PLUS}
| "-" {MINUS}
| "*" {TIMES}
| "(" {LPAREN}
| ")" {RPAREN}
| "{" {LBRACE}
| "}" {RBRACE}
| ":" {COLON}
| "=" {EQ}
| "!=" {NEQ}
| "<=" {LEQ}
| "<" {LT}
| ">=" {GEQ}
| "->" {ARROW}
| "==>" {IMPL}
| ">" {GT}
| "0" {INT(0)}
| "&&" {AND}
| "||" {OR}
| "|" {VLINE}
| "let" {LET}
| "rec" {REC}
| "not" {NOT}
| "in" {IN}
| "int" {INTEGER}
| "true" {TRUE}
| "false" {FALSE}
| "if" {IF}
| "then" {THEN}
| "else" {ELSE}
| "assert" {ASSERT}
| "fail" {FAIL}
| "select" {SELECT}
| "when" {WHEN}
| "mod" {MOD}
| digitnz digit*
   {let s = Lexing.lexeme lexbuf in INT(int_of_string s)}
| minus digitnz digit*
   {let s = Lexing.lexeme lexbuf in INT(int_of_string s)}
| (lower|higher) (mark|digit|higher|lower)*
    { let s = Lexing.lexeme lexbuf in ID(s)}
| eof { EOF }
| "(*" { comment lexbuf; token lexbuf }
| _
    { Format.eprintf "unknown token %s in line %d, column %d-%d @."
	(Lexing.lexeme lexbuf)
        (!line_no)
	((Lexing.lexeme_start lexbuf)- (!end_of_previousline))
	((Lexing.lexeme_end lexbuf)-(!end_of_previousline));
      failwith "lex error" }

(* For nested comments. *)
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf; comment lexbuf }
| eof
    {  print_string "Lex error: unterminated comment\n";
       failwith "unterminated comment" }
| _
    { comment lexbuf }

{
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
}
