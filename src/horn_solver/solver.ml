(** Runs a solver to find a solution for some Horn clauses. *)

open Lib

(** Solver's pid. *)
let solver_pid = ref None
(** Solver's stdout. *)
let stdout = ref None
(** Solver's stderr. *)
let stderr = ref None
(** Solver's stdin. *)
let stdin: Format.formatter option ref = ref None

let rec print_stderr () = try (
  match ! stderr with
  | None -> ()
  | Some stderr ->
    Format.printf "%s@." (input_line stderr) ;
    print_stderr ()
) with _ -> ()
let rec print_stdout () = try (
  match ! stdout with
  | None -> ()
  | Some stdout ->
    Format.printf "%s@." (input_line stdout) ;
    print_stdout ()
) with _ -> ()


(** Kills the solver if any. *)
let kill () = (fun () ->
  match ! solver_pid with
  | Some pid -> Unix.kill pid 9
  | _ -> ()
) |> sanitize "while killing horn clause solver"


(** Signal handler that kills the solver. *)
let kill_handle = Sys.Signal_handle(
  fun _ -> kill () |> Res.unwrap "while killing solver"
)


(** Spawns the solver. *)
let spawn () = (fun () ->
  Sys.set_signal Sys.sigalrm kill_handle ;
  Sys.set_signal Sys.sigint  kill_handle ;
  Sys.set_signal Sys.sigquit kill_handle ;
  Sys.set_signal Sys.sigterm kill_handle ;
  Sys.set_signal Sys.sigpipe kill_handle ;

  (* Initialize pipes. *)
  let (
    (solver_stdin_in,  solver_stdin_out ),
    (solver_stdout_in, solver_stdout_out),
    (solver_stderr_in, solver_stderr_out)
  ) = ( Unix.pipe (), Unix.pipe (), Unix.pipe () )
  in
  (* Spawn solver. *)
  let pid =
    match ! Conf.clause_solver with
    | (bin :: _) as cmd ->
      Unix.create_process
        bin
        (cmd |> Array.of_list)
        solver_stdin_in
        solver_stdout_out
        solver_stderr_out
    | [] -> Failure "empty solver command" |> raise
  in
  (* Close useless pipes. *)
  Unix.close solver_stdin_in ;
  Unix.close solver_stdout_out ; 
  Unix.close solver_stderr_out ;

  (* Remember pid. *)
  solver_pid := Some pid ;
  stdout := Some (
    Unix.in_channel_of_descr solver_stdout_in
  ) ;
  stderr := Some (
    Unix.in_channel_of_descr solver_stderr_in
  ) ;
  let solver_stdin = 
    Unix.out_channel_of_descr solver_stdin_out
    |> Format.formatter_of_out_channel
  in
  stdin := Some solver_stdin ;

  solver_stdin
) |> sanitize "while spawning horn clause solver"


let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Format.fprintf outx "%s:%d:%d" pos.Lexing.pos_fname pos.Lexing.pos_lnum (
    pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1
  )


(** Parses the output of the solver. *)
let parse lexbuf =
  try
    (SmtParse.top SmtLex.token lexbuf, lexbuf) |> Res.ok
  with
  | SmtLex.SyntaxError msg -> Res.err [
    Format.asprintf "%a: %s" print_position lexbuf msg
  ]
  | SmtParse.Error -> Res.err [
    Format.asprintf
      "%a: syntax error on character '%c' (%b)" print_position lexbuf
      (Lexing.lexeme_char lexbuf lexbuf.Lexing.lex_last_pos)
      lexbuf.Lexing.lex_eof_reached
  ]
  | e -> Res.err [
    "unexpected error during parsing" ;
    Format.asprintf "%s" (Printexc.to_string e)
  ]

let start_parsing stdout =
  let lexbuf = Lexing.from_channel stdout in
  parse lexbuf


(** Spawns the solver, feeds it the clauses, and parses its output before
closing it. *)
let solve filename clauses =
  let res =
    if ! Conf.verb then Format.printf "  spawning solver...@." ;
    spawn ()
    |> Res.and_then (fun stdin ->
      (fun () ->
        if ! Conf.verb then Format.printf "  printing clauses...@." ;
        let res = Cond.ToSmt2.clauses_to_smt2 stdin false filename clauses in
        ( match ! solver_pid with
          | Some pid ->
            if ! Conf.verb then
              Format.printf "  waiting for solver to terminate...@." ;
            let _ = Unix.waitpid [] pid in
            solver_pid := None ;
            ()
          | None -> ()
        ) ;
        res
      )
      |> sanitize "waiting for the solver to terminate"
      |> Res.map (fun () -> stdin)
    )
    |> Res.and_then (
      fun stdin -> (
        if ! Conf.verb then Format.printf "  parsing result...@." ;
        match ! stdout with
        | Some stdout -> start_parsing stdout |> Res.and_then (
          function
          | (ParseBase.Sat, lex) ->
            if ! Conf.verb then Format.printf "    sat@." ;
            Res.ok (true, lex)
          | (ParseBase.Unsat, lex) ->
            if ! Conf.verb then Format.printf "    unsat@." ;
            Res.ok (false, lex)
          | (res, _) -> Res.err [
            ParseBase.desc_of res
            |> Format.sprintf "expected sat or unsat, got %s"
          ]
        )
        | None -> Res.err ["cannot access solver's stdout"]
      )
    )
    |> Res.chain_err "while retrieving sat result"
    |> Res.and_then(function
      | (true, lex) ->
        if ! Conf.verb then Format.printf "  retrieving model...@." ;
        parse lex |> Res.map fst |> Res.and_then(
          function
          | ParseBase.Model model -> Some model |> Res.ok
          | parse_res -> Res.err [
            ParseBase.desc_of parse_res
            |> Format.sprintf "expected model, got %s"
          ]
        )
        |> Res.chain_err "while retrieving model"
      | (false, _) -> Res.ok None
    )
  in
  if ! Conf.verb then Format.printf
    "  cleaning up...@." ;
  let kill_res = kill () in
  if Res.is_ok kill_res || Res.is_err res then
    res
  else kill_res |> Res.map (fun _ -> None)
