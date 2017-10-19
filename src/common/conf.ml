(** Settings of the run. *)


(** Effect analysis flag. *)
let effect_analysis = ref true

(** Whether or not to run the solver. *)
let run_solver = ref true

(** Command running the solver. *)
let clause_solver = ref "hoice"

(** Options to pass to the solver. *)
let clause_solver_opts = ref []

(** Caml file we're analyzing. *)
let ml_file = ref None

(** Verbose flag. *)
let verb = ref false


(** CLAP stuff. *)
module Clap = struct

  open Format
  open Core
  open Lib

  type 'a parse_res = ('a, (string * string)) Res.res

  let parse_res_chain msg = function
  | Res.Ok res -> Res.Ok res
  | Res.Err (arg, msg') -> Res.Err (
    arg, Format.sprintf "%s\n%s" msg msg'
  )

  let is_okay = function
  | Res.Ok _ -> true
  | _ -> false

  let res_map f = function
  | Res.Ok res -> Res.Ok (f res)
  | Res.Err err -> Res.Err err

  let bool_to_str = function
  | true -> "on"
  | false -> "off"
  let bool_format = "[on|true|off|false]"
  let bool_validator = function
  | "on" | "true" -> Res.Ok true
  | "off" | "false" -> Res.Ok false
  | arg -> Res.Err (
    arg, sprintf "expected boolean argument `%s`" bool_format
  )

  let uargs = [
    ( "-v", ("verbose output", fun () -> verb := true) )
  ]

  let args = [
    ( "--effect_analysis",
      "(de)activates effect analysis",
      bool_format,
      bool_to_str ! effect_analysis,
      fun arg -> bool_validator arg |> res_map (
        fun b -> effect_analysis := b
      )
    ) ;
    ( "--infer",
      "(de)activates inference (prints the clauses on stdout if off)",
      bool_format,
      bool_to_str ! run_solver,
      fun arg -> bool_validator arg |> res_map (
        fun b -> run_solver := b
      )
    ) ;
    ( "--solver",
      "command running the horn clause solver, e.g. `hoice` or `z3`",
      "<cmd>",
      ! clause_solver,
      fun arg -> clause_solver := arg ; Res.Ok ()
    ) ;
  ]

  let help_format_len = 30

  let print_help () =
    Format.printf "\
      Usage: fp_mc [options]* <caml_file> [-- <solver arguments>*]@.  \
        the arguments passed after the '--' are passed to the underlying \
        horn clause solver@.\
      Options:@.\
    " ;
    List.iter uargs ~f:(
      fun (opt, (desc, _)) ->
        Format.printf
          "  @[<v>%-20s %-20s %s@]@." (Format.sprintf " %s" opt) "" desc
    ) ;
    List.iter args ~f:(
      fun (opt, desc, fmt, default, _) ->
        Format.printf
          "  @[<v>%-20s %-20s %s@   default '%s'@]@." opt fmt desc default
    ) ;
    ()


  let try_clap = function
  | Res.Ok res -> res
  | Res.Err (arg, msg) -> (
    print_help () ; 
    Format.printf "@.\
      Error during command-line argument parsing on '%s':@.  %s\
    " arg msg ;
    exit 2
  )

  let run () =
    let rec loop = function
    | "-h" :: tail | "--help" :: tail -> (
      print_help () ;
      exit 0
    )
    | [ file ] -> ml_file := Some file ; Res.Ok ()
    | file :: "--" :: tail ->
      ml_file := Some file ;
      clause_solver_opts := tail ;
      Res.Ok ()
    | opt :: tail when (
      List.find uargs ~f:( fun (o, _) -> o = opt ) <> None
    ) -> (
      match List.find uargs ~f:( fun (o, _) -> o = opt ) with
      | Some (_, (_, action)) -> action () ; loop tail
      | None -> failwith "unreachable"
    )
    | arg :: value :: tail -> (
      match List.find args ~f:(
        fun (opt, _, _, _, _) -> opt = arg
      ) with
      | Some (opt, _, _, _, action) ->
        let res = action value |> parse_res_chain (
          sprintf "on option '%s'" arg
        ) in
        if is_okay res then loop tail else res
      | None ->
        if String.sub arg 0 1 = "-" then
          Res.Err (arg, "unknown option")
        else (
          ml_file := Some arg ;
          match tail with
          | "--" :: opts -> clause_solver_opts := opts ; Res.Ok ()
          | unexpected :: _ -> Res.Err (
            unexpected,
            sprintf
              "expected optional '--' and solver options after file '%s'" arg
          )
          | [] -> Res.Ok ()
        )
    )
    | [] -> Res.Ok ()
    in
    let res =
      Array.sub Sys.argv ~pos:1 ~len:(Array.length Sys.argv - 1)
      |> Array.to_list |> loop
    in
    ( if is_okay res then match ! ml_file with
      | Some f -> Res.Ok f
      | None -> Res.Err (
        "<caml_file>", "expected path to caml file, found nothing"
      )
      else res |> res_map (fun _ -> "unused")
    ) |> try_clap


end



(** Runs clap and initializes the configuration. *)
let init () =
  Sys.catch_break true ;
  Clap.run ()