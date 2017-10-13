open Core


module ToSmt2 = struct

  let main
    verbose filename qualfilename do_effect_analysis
  =
    if verbose then (
      Format.printf "; loading file...@."
    ) ;
    let { Loader.horn_clauses ; Loader.type_env ; } =
      Loader.main Loader.Config.({
        filename ; qualfilename ;
        reduce_vc = false ; allow_to_reduce_multi_impl = false ;
        do_effect_analysis
      })
    in
    (* Format.printf "%s@." (Type.Env.to_string type_env) ; *)
    if verbose then (
      Format.printf "; done loading, translating...@."
    ) ;
    Cond.ToSmt2.print_clauses verbose filename horn_clauses ;
    ()

end


module Clap = struct
  open Format

  type ('a, 'err) res =
  | Ok of 'a
  | Err of 'err
  
  type 'a parse_res = ('a, (string * string)) res

  let parse_res_chain msg = function
  | Ok res -> Ok res
  | Err (arg, msg') -> Err (
    arg, Format.sprintf "%s\n%s" msg msg'
  )

  let is_okay = function
  | Ok _ -> true
  | _ -> false

  let res_map f = function
  | Ok res -> Ok (f res)
  | Err err -> Err err

  let bool_to_str = function
  | true -> "on"
  | false -> "off"
  let bool_format = "[on|true|off|false]"
  let bool_validator = function
  | "on" | "true" -> Ok true
  | "off" | "false" -> Ok false
  | arg -> Err (
    arg, sprintf "expected boolean argument `%s`" bool_format
  )

  let effect_analysis = ref true
  let run_solver = ref true
  let clause_solver = ref "hoice"
  let clause_solver_opts = ref []
  let ml_file = ref None

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
      "(de)activates inference, simply prints the clauses if off",
      bool_format,
      bool_to_str ! run_solver,
      fun arg -> bool_validator arg |> res_map (
        fun b -> run_solver := b
      )
    ) ;
    ( "--solver",
      "command running the horn clause solver",
      "<cmd>",
      ! clause_solver,
      fun arg -> clause_solver := arg ; Ok ()
    ) ;
  ]

  let help_format_len = 30

  let print_help () =
    printf "\
      Usage: fp_mc [options]* <caml_file> [-- <solver arguments>*]@.  \
        the arguments passed after the '--' are passed to the underlying \
        horn clause solver@.\
      Options:@.\
    " ;
    List.iter args ~f:(
      fun (opt, desc, fmt, default, _) ->
        printf "  @[<v>%-20s %-20s %s@   default '%s'@]@." opt fmt desc default
    )


  let try_clap = function
  | Ok res -> res
  | Err (arg, msg) -> (
    print_help () ; 
    printf "@.\
      Error during command-line argument parsing on '%s'@.%s\
    " arg msg ;
    exit 2
  )

  let run () =
    let rec loop = function
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
          Err (arg, "unknown option")
        else (
          ml_file := Some arg ;
          match tail with
          | "--" :: opts -> clause_solver_opts := opts ; Ok ()
          | unexpected :: _ -> Err (
            unexpected,
            sprintf
              "expected optional '--' and solver options after file '%s'" arg
          )
          | [] -> Ok ()
        )
    )
    | [ file ] -> ml_file := Some file ; Ok ()
    | [] -> Ok ()
    in
    let res =
      Array.sub Sys.argv ~pos:1 ~len:(Array.length Sys.argv - 1)
      |> Array.to_list |> loop
    in
    ( if is_okay res then match ! ml_file with
      | Some f -> Ok f
      | None -> Err (
        "<caml_file>", "expected path to caml file, found nothing"
      )
      else res |> res_map (fun _ -> "unused")
    ) |> try_clap
end




let run () =
  let ml_file = Clap.run () in
  Sys.catch_break true ;
  let status =
    try (
      let qualfilename =
        Filename.chop_extension ml_file ^ ".quals"
      in
      ToSmt2.main
        false
        ml_file qualfilename
        ! Clap.effect_analysis ;
      0
    ) with Sys.Break -> 1
  in
  exit status

let _ = run ()
