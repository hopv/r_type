open Program
open Core

exception ConvertError

type t = Cond.t
module RefType = Type.RefType

let fold_exn ~f = function
| (x :: xs) -> List.fold xs ~f ~init:x
| _ -> failwith "unexpected"

let make_type tyenv term condition =
  if Cond.is_var term
  then
    let vtype = Type.Env.find_exn tyenv (Cond.vid_exn term) in
    if Type.RefType.is_base vtype
    then  Type.RefType.from_condition ~f:condition
    else vtype
  else Type.RefType.from_condition ~f:condition

let rec gen tyenv exp : Cond.t * Type.Extended.t =
  try
    begin
      match exp with
      (* | Exp.Term term when Cond.is_var term -> (Cond.true_, Type.T.(Cond.true_ ==> Type.Env.find_exn tyenv (Cond.vid_exn term))) *)
      | Exp.Term term ->
          (Cond.true_, Type.T.(Cond.true_ ==> make_type tyenv term (fun v -> Cond.T.(var v == term))))
      | Exp.Fail ->
          (* Logger.debug "fail";
          Type.Env.log tyenv;
          Cond.log (Type.Env.denote tyenv); *)
          (Cond.T.(Type.Env.denote tyenv ==> false_), Type.lift Type.RefType.top)
      | Exp.Branch choices ->
          let iter (cond, exp) =
            let (const, ty) = gen Type.Env.T.(tyenv @<< from_condition cond) exp in
            (const, Type.T.(cond ==>& ty))
          in
          let (constraints, types) = List.map choices ~f:iter |> List.unzip in
          (fold_exn constraints ~f:Cond.and_, fold_exn types ~f:Type.and_)
      | Exp.Let_ (vid, exp1, exp2) ->
          (match exp1 with
          | Exp.Term term ->
              let term_type = make_type tyenv term (fun v -> Cond.T.(var v == term)) in
              let (constraint_, new_type) = gen Type.Env.T.(tyenv @<< from_map (vid, term_type)) exp2 in
              (constraint_, Type.T.(Cond.T.(var vid == term) ==>& new_type))
          | Exp.App (vid1, vid2) ->
              let fun_type = Type.Env.find_exn tyenv vid1 in
              let arg_type = Type.Env.find_exn tyenv vid2 in
              let fun_type_arg = Type.RefType.arg fun_type in
              let rtn_type =
                let fun_type_rtn = Type.RefType.rtn fun_type in
                let fun_type_vid = Type.RefType.vid fun_type in
                Type.RefType.subst fun_type_rtn fun_type_vid vid2 in
              let (constraint1, new_type) = gen Type.Env.T.(tyenv @<< from_map (vid, rtn_type)) exp2 in
              let new_type' = Type.T.(Type.RefType.denote vid rtn_type ==>& new_type) in
              let constraint2 =
                let arg_type = Type.RefType.subst_nu arg_type vid2 in
                let fun_type_arg = Type.RefType.subst_nu fun_type_arg vid2 in
                Cond.Horn.hornize (Type.Ord.denote tyenv (Type.lift arg_type, fun_type_arg)) in
              (Cond.T.(constraint1 && constraint2), new_type')
          | Exp.Fail ->
              (Cond.T.(Type.Env.denote tyenv ==> false_), Type.lift Type.RefType.top)
          | _ -> failwith "unexpected"
            )
      | _ -> failwith "illigal pattern exp"
    end
  with
  | e -> Exn.reraise e "verification condition calculation failed"

let vc tyenv (Program (fs)) =
  let vc_f ({ Func.name ; Func.args ; Func.exp ; _ } : Func.t) =
    let ftype = Type.Env.find_exn tyenv name in
    let var_types =
      let rec to_list xs t =
        match xs with
            [] -> []
          | x :: xs' ->
            match t with
              Type.RefType.Func (v, arg_type, rtn_type) -> (x, arg_type) :: to_list xs' (Type.RefType.subst rtn_type v x)
            | _ -> (x, t) :: []
      in to_list args ftype
    in
    let tyenv' = List.fold var_types ~init:tyenv ~f:(fun tyenv tuple -> Type.Env.T.(tyenv @<< from_map tuple)) in
    let (cond, exty) = gen tyenv' exp in
    let exty' = List.fold var_types ~init:exty ~f:(fun acc (vid, ty) -> Type.T.(Type.RefType.denote vid ty ==>& acc)) in
    let rtn_type_expected =
      let rec get_rtn_type xs t =
        match xs with
            [] -> t
          | x :: xs' ->
            match t with
              Type.RefType.Func (v, _arg_type, rtn_type) -> get_rtn_type xs' (Type.RefType.subst rtn_type v x)
            | _ -> t
      in get_rtn_type args ftype
    in
    Cond.T.(cond && Cond.Horn.hornize (Type.Ord.denote tyenv (exty', rtn_type_expected)))
  in
  List.fold fs ~init:Cond.true_ ~f:(fun cond f -> Cond.T.(cond && vc_f f))

let main tyenv program  = vc tyenv program
