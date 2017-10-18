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
    Unix.create_process
      ! Conf.clause_solver
      (Array.of_list ! Conf.clause_solver_opts)
      solver_stdin_in
      solver_stdout_out
      solver_stderr_out
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



let solve filename clauses =
  spawn ()
  |> Res.and_then (fun stdin ->
    (fun () -> Cond.ToSmt2.clauses_to_smt2 stdin false filename clauses)
    |> sanitize "while printing clauses to solver's stdin"
  )
  |> Res.and_then (
    fun _ -> Res.err ["solving is not implemented yet"]
  )
