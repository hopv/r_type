{
open SmtParse
open Lexing

let line_no = ref 1  (* the current line number, used for error reporting *)
let end_of_previousline = ref 0
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with
      pos_bol  = lexbuf.lex_curr_pos ;
      pos_lnum = pos.pos_lnum + 1
  }
}

(* abbreviations *)
let space = [' ' '\t' '\r']
let newline = ['\n']
let digit = ['0'-'9']
let digitnz = ['1'-'9']
let lcase = ['a'-'z']
let hcase = ['A'-'Z']
let idlegal = [
  '~' '!' '@' '$' '%' '^' '&' '*' '_' '-' '+' '=' '<' '>' '.' '?' '/'
]

(* The main part, defining tokens and the corresponding actions *)
rule token = parse
| newline {
  next_line lexbuf ;
  token lexbuf
}

| space+ { token lexbuf }
| ";" { comment lexbuf }

| "unsat" {UNSAT}
| "sat" {SAT}
| "model" {MODEL}
| "define-fun" {DEFINE}
| "exists" {EXISTS}
| "let" {LET}

| "Int" {INT}
| "Bool" {BOOL}

| "(" {OPAREN}
| ")" {CPAREN}

| "=>" {OP (Lexing.lexeme lexbuf)}
| "and" {OP (Lexing.lexeme lexbuf)}
| "or" {OP (Lexing.lexeme lexbuf)}
| "not" {OP (Lexing.lexeme lexbuf)}

| "=" {OP (Lexing.lexeme lexbuf)}
| "ite" {OP (Lexing.lexeme lexbuf)}

| "<=" {OP (Lexing.lexeme lexbuf)}
| "<" {OP (Lexing.lexeme lexbuf)}
| ">=" {OP (Lexing.lexeme lexbuf)}
| ">" {OP (Lexing.lexeme lexbuf)}

| "+" {OP (Lexing.lexeme lexbuf)}
| "-" {OP (Lexing.lexeme lexbuf)}
| "*" {OP (Lexing.lexeme lexbuf)}
| "/" {OP (Lexing.lexeme lexbuf)}

| digit+ { CINT (Lexing.lexeme lexbuf) }
| "true" { CBOOL (Lexing.lexeme lexbuf) }
| "false" { CBOOL (Lexing.lexeme lexbuf) }

| "\"" { dquoted (Buffer.create 17) lexbuf }
| "|" { piped (Buffer.create 17) lexbuf }
| (lcase|hcase) (lcase|hcase|idlegal|digit)* { IDENT (Lexing.lexeme lexbuf) }

| eof { EOF }
| _
    { Format.eprintf "unknown token %s in line %d, column %d-%d @."
	(Lexing.lexeme lexbuf)
        (!line_no)
	((Lexing.lexeme_start lexbuf)- (!end_of_previousline))
	((Lexing.lexeme_end lexbuf)-(!end_of_previousline));
      failwith "lex error" }

and comment = parse
| newline { next_line lexbuf ; token lexbuf }
| _ { comment lexbuf }

and dquoted buf = parse
| "\"" { DQUOTED (Buffer.contents buf) }
| newline {
  next_line lexbuf ;
  Buffer.add_char buf '\n' ;
  dquoted buf lexbuf
}
| [^ '|' ]+ {
  Buffer.add_string buf (Lexing.lexeme lexbuf) ;
  dquoted buf lexbuf
}
| _ {
  raise (
    SyntaxError (
      "Illegal string character: " ^ Lexing.lexeme lexbuf
    )
  )
}
| eof { raise (SyntaxError ("unterminated string")) }

and piped buf = parse
| "|" { IDENT (Buffer.contents buf) }
| newline {
  next_line lexbuf ;
  Buffer.add_char buf '\n' ;
  piped buf lexbuf
}
| [^ '|' ]+ {
  Buffer.add_string buf (Lexing.lexeme lexbuf) ;
  piped buf lexbuf
}
| _ {
  raise (
    SyntaxError (
      "Illegal piped ident character: " ^ Lexing.lexeme lexbuf
    )
  )
}
| eof { raise (SyntaxError ("unterminated piped ident")) }

{
  let read filename = Lexing.from_channel (open_in filename)
  let main filename = token (read filename)
}
