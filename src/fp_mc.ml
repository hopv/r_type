open Core


module ToSmt2 = struct

  let main verbose filename qualfilename reduce_vc allow_to_reduce_multi_impl do_effect_analysis =
    if verbose then (
      Format.printf "; loading file...@."
    ) ;
    let { Loader.horn_clauses ; Loader.type_env ; } =
      Loader.main Loader.Config.({
        filename ; qualfilename ; reduce_vc ; allow_to_reduce_multi_impl ; do_effect_analysis
      })
    in
    (* Format.printf "%s@." (Type.Env.to_string type_env) ; *)
    if verbose then (
      Format.printf "; done loading, translating...@."
    ) ;
    Cond.ToSmt2.print_clauses verbose filename horn_clauses ;
    exit 0

end

module Commands = struct
  let bootstrap f =
    Sys.catch_break true;
    let status = try f (); 0 with
      Sys.Break -> 1
    in exit status

  let to_hc_command =
    Command.basic
      ~summary:"\
        Translate a ml program verification problem to SMT-LIB 2 horn clauses\
      "
      Command.Spec.(empty
        +> flag "--no-vc-reduction" no_arg ~doc:" Disable VC reduction"
        +> flag "--simple-vc" no_arg ~doc:" Disallow to reduce consequent unknowns of multiple vcs"
        +> flag "--no-effect-analysis" no_arg ~doc:" Disables effect analysis"
        +> anon ("filename" %: file)
      )
      (fun
          no_vc_reduction
          disallow_to_reduce_multi_impl
          no_effect_analysis
          filename
          () ->
        bootstrap (fun () ->
          let qualfilename = Filename.chop_extension filename ^ ".quals" in
          ToSmt2.main
            false
            filename qualfilename
            (not no_vc_reduction) (not disallow_to_reduce_multi_impl) (not no_effect_analysis)
        )
      )

  let commands =
    Command.group
      ~summary:"\
        A model-checker for caml programs.\
      " [
        "to-hc", to_hc_command ;
      ]
end





let run () =
  Format.printf "Running...@." ;
  Command.run ~version:"0.1" Commands.commands ;
  Format.printf "done@."

let _ = run ()
