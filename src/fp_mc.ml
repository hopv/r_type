open Core
open Lib

let work filename =
  Loader.main Loader.Config.(
    { filename ; qualfilename = "" ;
      reduce_vc = false ;
      allow_to_reduce_multi_impl = false ;
      do_effect_analysis = ! Conf.effect_analysis
    }
  )

  |> Res.and_then (fun { Loader.horn_clauses } ->

    if ! Conf.run_solver |> not then
      (fun () ->
        Cond.ToSmt2.clauses_to_smt2
          Format.std_formatter false filename horn_clauses
      ) |> sanitize "during horn clause generation"
    else
      let res =
        Solver.solve filename horn_clauses
        |> Res.chain_err "during horn clause solving"
      in
      let kill_res = Solver.kill () in
      if Res.is_ok res then kill_res else res

  )



let _ =
  let ml_file = Conf.init () in
  work ml_file |> Res.unwrap ~finalize:(
    fun () -> Solver.kill () |> Res.unwrap "while killing solver"
  ) (Format.asprintf "on file %s" ml_file) ;
  exit 0