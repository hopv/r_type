open Core
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let main filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let tyenv =
    try Fmlparser.main Fmllexer.token lexbuf with
    | Fmllexer.SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      failwith "SyntaxError"
    | Fmlparser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      failwith "SyntaxError"
  in
  In_channel.close inx;
  Type.Env.ty_map tyenv ~f:(fun ty -> ty |> Type.RefType.fresh)
