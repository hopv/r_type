(** Settings of the run. *)

open Core

(** Effect analysis flag. *)
let effect_analysis = ref true

(** Whether or not to run the solver. *)
let run_solver = ref true

(** Command and options running the solver. *)
let clause_solver = ref [ "hoice" ]
(** String version of `clause_solver`. *)
let clause_solver_str () =
  String.concat ~sep:" " ! clause_solver

(** Caml file we're analyzing. *)
let ml_file = ref None

(** Verbose flag. *)
let verb = ref false


(** CLAP stuff. *)
module Clap = struct

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

  (* Nullary arguments (flags). *)
  let n_args = [
    ( "-v", ("verbose output", fun () -> verb := true) )
  ]

  (* Unary arguments (options). *)
  let u_args = [
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
      clause_solver_str (),
      fun arg ->
        clause_solver := (
          let split = String.split_on_chars arg ~on:[ ' ' ; '\t' ; '\r' ] in
          List.fold_left split ~init:[] ~f:(
            fun acc s ->
              let s = String.strip s in
              if not (String.is_empty s) then (
                (* Format.printf "%s@." s ; *)
                s :: acc
              ) else acc
          )
          |> List.rev
        ) ;
        Res.ok ()
    ) ;
  ]

  let help_format_len = 30

  let print_help () =
    Format.printf "\
      Usage: r_type [options]* <caml_file>@.\
      NB: r_type verifies that function `main` from the input caml@.    \
          file never fails. Hence, make sure that entry point of your@.    \
          program is a function called `main`.@.\
      Options:@.\
    " ;
    List.iter n_args ~f:(
      fun (opt, (desc, _)) ->
        Format.printf
          "  @[<v>%-20s %-20s %s@]@." (Format.sprintf " %s" opt) "" desc
    ) ;
    List.iter u_args ~f:(
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
    let first_is (expected: string) ((got, _): string * 'a): bool = String.equal expected got in

    let rec loop = function
        | "-h" :: _tail | "--help" :: _tail -> (
          print_help () ;
          exit 0
        )

        | [ file ] -> ml_file := Some file ; Res.Ok ()

        (* This case is actually necessarily an error **for now**. The error
        will be caught after, when checking that the file is not `None. *)
        | [] -> Res.Ok ()

        | arg :: value :: tail -> (
            match List.find n_args ~f:(first_is arg) with
            | Some (_, (_, action)) -> (
                action ();
                value :: tail |> loop
            )
            | None -> (
                match List.find u_args ~f:(fun (opt, _, _, _, _) -> String.equal opt arg) with
                | Some (_opt, _, _, _, action) -> (
                    let res =
                      action value |> parse_res_chain (
                        sprintf "on option '%s'" arg
                      )
                    in
                    if is_okay res then loop tail else res
                )
                | None -> Res.Err (arg, "unknown option")
            )
        )
    in

    let argv = Sys.get_argv () in
    let res =
      Array.sub argv ~pos:1 ~len:(Array.length argv - 1)
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
