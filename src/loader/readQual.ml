open Core
open Data
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let main filename : Cond.t list =
  match Sys.file_exists ~follow_symlinks:true filename with
  | `Yes ->
    let inx = In_channel.create filename in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let quals =
      try Qualparser.main Quallexer.token lexbuf with
      | Quallexer.SyntaxError msg ->
        fprintf stderr "%a: %s\n" print_position lexbuf msg;
        failwith "SyntaxError"
      | Qualparser.Error ->
        fprintf stderr "%a: syntax error\n" print_position lexbuf;
        failwith "SyntaxError"
    in
    In_channel.close inx;
    quals |> List.map ~f:Cond.inspect
  | _ ->
    Logger.warn ("This file does not exist: " ^ filename); []
