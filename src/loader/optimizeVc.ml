open Core

let not_true t =
  match Cond.decomp_hc t with
  | _, Cond.Value (Objt.BoolObj true) -> false
  | _ -> true

module RemoveNonRecursiveUnknown = struct
  let rec run ?(reports = []) (clauses : Cond.Horn.t) (reduce_multi_impl : bool) : (VcControl.DetectNonRecursiveUnknown.Report.t list * Cond.Horn.t) =
    let new_reports = VcControl.DetectNonRecursiveUnknown.run clauses reduce_multi_impl in
    match VcControl.DetectNonRecursiveUnknown.Report.select_effective_one new_reports with
    | None -> (reports, clauses)
    | Some new_one ->
      let new_clauses = Cond.Horn.map clauses ~f:(VcControl.DetectNonRecursiveUnknown.Report.apply_to_cond new_one) |> List.concat |> List.map ~f:Cond.simplify_alpha in
      let reports = List.map reports ~f:(VcControl.DetectNonRecursiveUnknown.Report.apply_to_report new_one) in
      run ~reports:(new_one :: reports) new_clauses reduce_multi_impl
end


let rec remove_non_recursive ?(reports = []) (clauses : Cond.Horn.t) (reduce_multi_impl : bool) : (VcControl.DetectNonRecursiveUnknown.Report.t list * Cond.Horn.t) =
  let new_reports = VcControl.DetectNonRecursiveUnknown.run clauses reduce_multi_impl in
  if new_reports = [] then
    reports, clauses
  else
    let rec apply new_reports (reports,clauses) =
      match new_reports with
      | [] -> reports, clauses
      | (un,conds as new_one)::new_reports' ->
          if List.for_all conds ~f:(fun cond -> VcControl.DetectNonRecursiveUnknown.Report.is_not_recursive un cond) then
            let new_clauses = Cond.Horn.map clauses ~f:(VcControl.DetectNonRecursiveUnknown.Report.apply_to_cond new_one) |> List.concat |> List.map ~f:Cond.simplify_alpha in
            let app = List.map ~f:(VcControl.DetectNonRecursiveUnknown.Report.apply_to_report new_one) in
            let reports = app reports in
            let new_reports'' = app new_reports' in
            apply new_reports'' (new_one :: reports, new_clauses)
          else
            apply new_reports' (reports, clauses)
    in
    let new_reports = VcControl.DetectNonRecursiveUnknown.Report.sort_by_count new_reports in
    let reports,clauses = apply new_reports (reports,clauses) in
    remove_non_recursive ~reports clauses reduce_multi_impl

let eliminate_unused_conds t =
  let fv = Cond.get_fv t in
  let body,head = Cond.decomp_hc ~full:true t in
  let body =
    let may_used t =
      match t with
      | Cond.Op2(Cond.Value (Objt.VarObj x), Op.Eq, Cond.Value (Objt.IntObj _ | Objt.BoolObj _)) ->
          1 < List.length @@ List.filter fv ~f:([%compare.equal: Identity.t] x)
      | _ -> true
    in
    List.filter body ~f:may_used
  in
  Cond.compose_hc body head


let eliminate_precond_of_safe_fun tyenv pr clauses =
  let preds = EffectInfer.assumed_as_true tyenv pr in
  let apps = List.concat_map clauses ~f:Cond.get_apps in
  let reports =
    let f app =
      let p = UnknownPredicate.id_of app in
      List.exists preds ~f:(Identity.equal p)
    in
    List.filter ~f apps
  in
  let clauses = List.fold reports ~init:clauses ~f:(fun clauses report -> Cond.Horn.map clauses ~f:(VcControl.DetectTruthyUnknown.Report.apply_to_cond report)) in
  reports, clauses


let run (clauses : Cond.Horn.t) (tyenv : Type.Env.t) (pr : Program.t) (do_effect_analysis : bool) : (Cond.Horn.t * Type.Env.t) =
  let clauses = List.concat_map ~f:Cond.flatten clauses in
  let truthy_reports, clauses =
    if do_effect_analysis then
      eliminate_precond_of_safe_fun tyenv pr clauses
    else
      [], clauses
  in
  let non_recursive_reports = [] in
  let tyenv =
    let reduced_un_reports = VcControl.ReducedUnknownReport.build truthy_reports non_recursive_reports in
    (VcControl.ReducedUnknownReport.remove_reduced_unknowns_from_tyenv reduced_un_reports tyenv) in
  clauses, tyenv
