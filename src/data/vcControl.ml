open Core
open Cond

type t = Cond.t

let rev_assign (cond : Cond.t) (un : Cond.UnknownApp.t) =
  let substs = Cond.UnknownApp.substs_of un in
  List.fold substs ~init:cond ~f:(fun acc (k, v) ->
    if Cond.is_var v then Cond.subst acc (Cond.vid_exn v) k
    else failwith "unexpected"
  )

let wrap_exists_free_vars (cond : Cond.t) (bindvars : Identity.Set.t) =
(*
  let cond = Cond.ElimFreeVar.run cond ~bind_vars:bindvars in
 *)
  let freevars = Identity.Set.diff (Cond.get_vids cond |> Identity.Set.of_list) bindvars in
  Identity.Set.fold freevars ~init:cond ~f:(fun cond vid ->
    Cond.op1 (Op.exists vid) cond
  )

module AlphaConvUnParams = struct
  module L = Label.Make(struct let label = "elim-dup-var" end)

  let run (cond : Cond.t) : Cond.t =
    let assume_eqs = ref [] in
    let new_cond = Cond.UApp_plus.map cond ~f:(fun un ->
      let uapp_conds =
        List.fold_right (Cond.UnknownApp.substs_of un) ~init:[] ~f:(fun (_kvid, vcond) uapp_conds ->
          let alpha_id = L.gen () in
          let () = assume_eqs := Cond.DSL.(var alpha_id == vcond) :: !assume_eqs in
          Cond.DSL.var alpha_id :: uapp_conds
        ) in
      Cond.uapp (Cond.UnknownApp.predicate_of un) uapp_conds
    ) in
    let new_cond' = Cond.DSL.(Cond.forall !assume_eqs (fun x -> x) ==> new_cond) in
    List.fold (Cond.get_vids new_cond' |> Identity.Set.of_list |> Identity.Set.to_list) ~init:new_cond' ~f:(fun cond vid ->
      Cond.subst cond vid (L.gen ())
    ) |> Horn.hornize
end

module DetectConsequentUnknown = struct
  module Report = struct
    type t = UnknownPredicate.t * Cond.t
  end

  let run (cond : Cond.t) : Report.t option =
    match cond with
    | Op2 (c1, Op.Impl, c2) ->
      Option.map (Cond.uapp_of c2) ~f:(fun un ->
        let cond = rev_assign c1 un in
        (Cond.UnknownApp.predicate_of un, cond)
      )
    | _ -> None
end

module DetectNonRecursiveUnknown = struct
  module Report = struct
    type t = UnknownPredicate.t * Cond.t list

    let is_not_recursive (un : UnknownPredicate.t) (cond : Cond.t) : bool =
      List.for_all (Cond.uapps_of cond) ~f:(fun (up, _) -> not (UnknownPredicate.equal un up))

    let can_be_single_horn (report : t) : bool =
      let (_, conds) = report in
      if List.length conds = 1 then true
      else
        if List.length conds > 1 then
          List.for_all conds ~f:(fun cond -> List.is_empty (Cond.uapps_of cond))
        else
          false

    let of_conseq_reports (creports : DetectConsequentUnknown.Report.t list) (allow_multi_impl : bool) : t list =
      let up_report_dict = UnknownPredicate.Map.of_alist_multi creports in
      let filtered_reports =
        UnknownPredicate.Map.to_alist up_report_dict |> List.filter ~f:(fun (un, conds) ->
          List.for_all conds ~f:(fun cond -> is_not_recursive un cond)
        ) |> (fun reports ->
          if allow_multi_impl
          then reports
          else List.filter reports ~f:can_be_single_horn
        )
      in
      List.map filtered_reports ~f:(fun ((un, conds) as report) ->
        let conds =
          if can_be_single_horn report
          then [Cond.exists conds (fun x -> x)]
          else conds
        in
        let (report : t) = (un, conds) in report
      )

    let has_specified_unknown (cond : Cond.t) (specified_un : UnknownPredicate.t) : bool =
      List.exists (Cond.uapps_of cond) ~f:(fun (up, _) -> UnknownPredicate.equal specified_un up)

    let apply_to_cond ((orig_un, u_conds) : t) (cond : Cond.t) : Cond.t list =
      let orig_un_arg_vids = UnknownPredicate.var_set_of orig_un in
      match cond with
      | Op2 (_c1, Op.Impl, c2) when Option.map (uapp_of c2) ~f:(fun un -> UnknownPredicate.equal (Cond.UnknownApp.predicate_of un) orig_un ) |> Option.value ~default:false
        -> []
      | _ ->
        if has_specified_unknown cond orig_un then
          Cond.UApp_plus.map_multi cond ~f:(fun uapp ->
            if UnknownPredicate.equal (Cond.UnknownApp.predicate_of uapp) orig_un then
              List.map u_conds ~f:(fun u_cond ->
                (* let _ = Cond.inspect ~tag:"cond before rename" u_cond in
                let _ = Logger.debug ~tag:"vids not to rename" (List.to_string (Identity.Set.to_list orig_un_arg_vids) ~f:(fun x -> x) ) in *)
                let u_cond_alpha_renamed = Cond.AlphaConv.run u_cond ~vids_not_to_rename:orig_un_arg_vids in
                (* let _ = Cond.inspect ~tag:"cond after rename" u_cond_alpha_renamed in *)
                Cond.UnknownApp.subst uapp u_cond_alpha_renamed
              )
            else
              [Cond.UnknownApp.to_cond uapp]
          ) |> List.map ~f:Horn.hornize
        else
          [cond]

    let apply_to_report (self : t) (target : t) : t =
      let (orig_un, u_conds) = self in
      let orig_un_arg_vids = UnknownPredicate.var_set_of orig_un in
      let (target_un, target_conds) = target in
      let target_conds =
        let do_subst cond sbst_cond =
          Cond.UApp_plus.map cond ~f:(fun uapp ->
            if UnknownPredicate.equal (Cond.UnknownApp.predicate_of uapp) orig_un then
              let sbst_cond_alpha_renamed = Cond.AlphaConv.run sbst_cond ~vids_not_to_rename:orig_un_arg_vids in
              Cond.UnknownApp.subst uapp sbst_cond_alpha_renamed
            else
              Cond.UnknownApp.to_cond uapp
          )
        in
        List.map target_conds ~f:(fun x ->
          if has_specified_unknown x orig_un then
            List.map u_conds ~f:(do_subst x)
          else
            [x]
        ) |> List.concat
      in
      (target_un, target_conds)

    let sort_by_count (reports : t list) : t list =
      let reports_with_count = List.map reports ~f:(fun (un, conds) ->
        (List.length conds, (un, conds))
      ) in
      let reports_with_count = List.sort reports_with_count ~compare:(fun a b -> Int.compare (Tuple.T2.get1 a) (Tuple.T2.get1 b)) in
      List.map reports_with_count ~f:Tuple.T2.get2

    let select_effective_one (reports : t list) : t option =
      let reports_with_count = List.map reports ~f:(fun (un, conds) ->
        (List.fold conds ~init:[] ~f:(fun acc x -> acc @ Cond.uapps_of x) |> List.length, (un, conds))
      ) in
      let reports_with_count = List.sort reports_with_count ~compare:(fun a b -> Int.compare (Tuple.T2.get1 a) (Tuple.T2.get1 b)) in
      List.hd reports_with_count |> Option.map ~f:Tuple.T2.get2
  end

  let run (clauses : Horn.t) (allow_multi_impl : bool) : Report.t list =
    let conseq_reports = Cond.Horn.map clauses ~f:DetectConsequentUnknown.run |> List.filter_opt in
    (* let () = List.iter conseq_reports ~f:DetectConsequentUnknown.Report.log in *)
    Report.of_conseq_reports conseq_reports allow_multi_impl
end

module DetectTruthyUnknown = struct
  module Report = struct
    type t = UnknownPredicate.t

    let apply_to_cond (self : t) (cond : Cond.t) : Cond.t =
      Cond.UApp_plus.map cond ~f:(fun uapp ->
        if UnknownPredicate.equal (Cond.UnknownApp.predicate_of uapp) self then
          Cond.true_
        else
          Cond.UnknownApp.to_cond uapp
      )
  end
end

module ReducedUnknownReport = struct
  type t = (UnknownPredicate.t * Cond.t) list

  let build (treports: DetectTruthyUnknown.Report.t list) (creports: DetectNonRecursiveUnknown.Report.t list) : t =
    List.map treports ~f:(fun x -> (x, Cond.true_)) @
    List.map creports ~f:(fun (un, conds) -> (un, List.map conds ~f:(fun cond -> wrap_exists_free_vars cond (UnknownPredicate.var_set_of un)) |> (fun conds -> Cond.exists conds (fun x -> x))))

  let remove_reduced_unknowns_from_tyenv (self : t) (tyenv : Type.Env.t) : Type.Env.t =
    let subst = UnknownPredicate.Map.of_alist_reduce self ~f:(fun x _ -> x) in
    Type.Env.ty_map tyenv ~f:(fun reftype -> Type.RefType.assign_unknown reftype subst)
end
