open Core
open Lib

module Map = Identity.Map
module Typ = Type.RefType


let print_type_env type_env model =
  Format.printf "Program is safe with types@.@." ;
  let types = Cond.UnknownSubst.Map.empty in
  Type.Env.assign_unknown type_env types |> Type.Env.to_string
  |> Format.printf "%s" ;
  if Stdlib.(model <> []) then (
    Format.printf "where@.@." ;
    List.iter model ~f:(
      Format.printf "@[<v>%a@]@." ParseBase.fmt_def
    )
  ) ;
  ()



let work filename =
  if ! Conf.verb then Format.printf "loading file '%s'...@." filename ;
  Loader.main Loader.Config.(
    { filename ; qualfilename = "" ;
      reduce_vc = false ;
      allow_to_reduce_multi_impl = false ;
      do_effect_analysis = ! Conf.effect_analysis
    }
  )

  |> Res.and_then (fun { Loader.horn_clauses ; Loader.type_env ; _ } ->

    if ! Conf.run_solver |> not then
      (fun () ->
        Cond.ToSmt2.clauses_to_smt2
          Format.std_formatter false filename horn_clauses
      ) |> sanitize "during horn clause generation"
    else (
      if ! Conf.verb then Format.printf "running solver...@." ;
      Solver.solve filename horn_clauses
      |> Res.chain_err "during horn clause solving"
      |> Res.and_then (function
        | Some model ->
          if ! Conf.verb then Format.printf "success, printing model...@.@." ;
          print_type_env type_env model ;
          Res.Ok ()
        | None ->
          Format.printf
            "This program is not typeable with refinement types@." ;
          Format.printf "It might be unsafe.@." ;
          Res.Ok ()
      )
    )

  )



let _ =
  let ml_file = Conf.init () in
  work ml_file |> Res.unwrap (Format.asprintf "on file %s" ml_file) ;
  exit 0
