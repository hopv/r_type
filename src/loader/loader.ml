open Core

module Config = struct
  type t = {
    filename : string;
    qualfilename : string;
    reduce_vc : bool;
    allow_to_reduce_multi_impl : bool;
    do_effect_analysis : bool;
  }
  [@@deriving create, eq, ord, hash]
end

type t = {
  type_env: Type.Env.t;
  unknown_predicates: UnknownPredicate.t list;
  horn_clauses: Cond.t list;
}
[@@deriving create, fields]

let (|->) (a : 'a) (f : 'a -> unit) : 'a = f a; a

let switch_use_refinement_annotation b = Program.use_refinement_annotation := b
let use_test_run = ref true
let switch_use_test_run b = use_test_run := b

let main (config : Config.t) = (fun () ->
  config.Config.filename |>
  MlLoader.parse |> MlLoader.desugar |>
  (fun pr ->
    let tyenv = Program.reftype_env_of pr in
    let fml = VerificationCondition.main tyenv pr in
    let clauses = Cond.Horn.main fml in
    let (clauses, tyenv) =
      if Config.(config.reduce_vc) || Config.(config.do_effect_analysis) then
        OptimizeVc.run
          clauses tyenv pr
          Config.(config.do_effect_analysis)
      else
        (clauses, tyenv)
    in
    let unknowns_of ty =
      List.map (Type.RefType.conds_of ty) ~f:Cond.uapps_of |> List.concat |> List.map ~f:Cond.UnknownApp.predicate_of in
    let unknown_predicates =
      List.map (Type.Env.types_of tyenv) ~f:unknowns_of |>
      List.concat |> List.dedup_and_sort ~compare:UnknownPredicate.compare
    in
    create
      ~type_env:tyenv
      ~unknown_predicates
      ~horn_clauses:clauses
      ()
  )
) |> Lib.sanitize "during caml loading"

let read_given_tyenv prfname tyfname =
  let program = prfname |> MlLoader.parse |> MlLoader.desugar in
  let tyenv = ReadFml.main tyfname in
  let fml = VerificationCondition.main tyenv program in
  let clauses = Cond.Horn.main fml in
  (tyenv, clauses)
