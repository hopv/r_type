open Core
open Sexplib.Std
module I = Formatable

let (=) = Poly.(=)

module Exp = struct
  type t =
    Let_ of Identity.t * t * t
  | Branch of (Cond.t * t) list
  | App of Identity.t * Identity.t
  | Term of Cond.t
  | Fail
  [@@deriving sexp, variants, eq, ord, hash]

  let var x = Term (Cond.var x)
  let value x = Term (Cond.value x)

  include Diggable.Make(struct
    type nonrec t = t
    let dig e f =
      match e with
        Let_ (fid, exp, lexp) -> Let_ (fid, f exp, f lexp)
      | Branch cs -> Branch (List.map cs ~f:(fun (cond, exp) -> cond, f exp))
      | _ as e -> e
  end)

  include Formatable.Make(struct
    type nonrec t = t
    let rec to_format = function
      | Let_ (vid, exp1, exp2) -> I.block [
          I.block [
            I.text ("let " ^ vid ^ " = ");
            to_format exp1;
            I.text " in";
          ];
          I.block [to_format exp2]
        ]
      | Branch (cs) ->
          I.block [
            I.line "case";
            I.indent (I.block
              (List.map cs ~f:(fun (cond, exp) ->
                I.block [
                  I.block [
                    I.text "when ";
                    Cond.to_format cond;
                    I.text " ==> "
                  ];
                  I.indent (to_format exp)
                ]
              ))
            )
          ]
      | App (fid, aid) ->
          I.text ("(" ^ fid ^ " " ^ aid ^ ")")
      | Term term ->
          Cond.to_format term
      | Fail ->
          I.text "fail"
  end)
end

module Func = struct
  module Annotation = struct
    module TypeSize = struct
      type t = Func of t * t | Value of int * int (* or, and *)
      [@@deriving variants, sexp, eq, ord, hash]
    end

    type t = { type_size: TypeSize.t option; reftype: Type.RefType.t option; }
    [@@deriving sexp, eq, ord, hash, fields]

    let make ?type_size ?reftype () : t = { type_size; reftype; }
    let empty = { type_size = None; reftype = None; }

    module Element = struct
      type t = RType of Type.RefType.t
      [@@deriving variants]

    end

    let from_elements (els : Element.t list) : t =
      List.fold els ~init:empty ~f:(fun an el ->
        let open Element in
        match el with
        | RType rt -> { an with reftype = Some rt }
      )
  end

  type t = { name: Identity.t; args: args; exp: Exp.t; annotation: Annotation.t }
  [@@deriving sexp, eq, ord, hash, fields]
  and args = Identity.t list
  [@@deriving sexp, eq, ord, hash]

  let is_main (self : t) = self.name = "main"

  let make ?annotation ~name ~args ~exp : t =
    let annotation = Option.value ~default:(Annotation.make ()) annotation in
    Fields.create ~name ~args ~exp ~annotation

  include Formatable.Make(struct
    type nonrec t = t
    let to_format { name; args; exp; _ } =
      I.block [
        I.block [
          I.inline (List.map (I.joint (name :: args) " ") ~f:(fun tx -> I.text tx));
          I.text " ="
        ];
        I.indent (I.block [Exp.to_format exp])
      ]
  end)
end

module Typedef = struct
  type t = Program of Func.t list
    [@@deriving sexp, eq, ord, hash]

  let to_format (Program recfuns) =
    I.block (List.map recfuns ~f:Func.to_format)

  let recfuns (Program recfuns) = recfuns
end
include Typedef
include Showsexp.Make(Typedef)
include Formatable.Make(Typedef)
(*
let type_env (Program fs) =
  let f acc (Func.RecFunc (fid, args, exp)) =
    let rec g = (function
    | v :: [] -> Type.RefType.bottom
    | v :: vs -> Type.RefType.func fid Type.RefType.bottom (g vs)
    ) in
    Type.Env.T.(acc @<< from_map (fid, g args)) in
  List.fold fs ~init:Type.Env.empty ~f
  *)

let to_func_map (program : Typedef.t) : Func.t Identity.Map.t =
  let funcs = recfuns program in
  List.fold funcs ~init:Identity.Map.empty ~f:(fun mp func ->
      match Identity.Map.add mp ~key:func.Func.name ~data:func with
      | `Ok mp -> mp
      | `Duplicate -> assert false
  )

module Info = struct
  type t = (Identity.t * Identity.t list) list
  [@@deriving sexp, eq, ord, hash]

  include Assocable.Make  (struct
    type nonrec t = t
    type key_t = Identity.t
    type value_t = Identity.t list

    let mapping obj = obj
  end)

  let from_program program =
    let from_recfun ({ Func.name ; Func.args ; _ } : Func.t) = (name, args) in
    match program with
  | Program (recfuns) -> List.map recfuns ~f:from_recfun

  let fids_of pinfo =
    List.map pinfo ~f:(fun (fid, _) -> fid)

  let fid_of_var pinfo vid =
    let (fid, _) =
      List.find_exn pinfo ~f:(fun (_, vids) -> List.exists vids ~f:(fun v -> v = vid)) in
    fid
end

module SimpleTypeInfer = struct
  open SimpleType

  let main (Program fs) =
    let rec (infer : Env.tt -> Relation.t -> Exp.t -> Env.tt * Relation.t * Essential.t) = fun tyenv tyrel exp ->
      let bind id ty = Relation.T.(Env.find_exn tyenv id == ty) in
      let bind_condition_variables tyrel cond =
        List.fold (Cond.get_vids cond) ~init:tyrel ~f:(fun tyrel vid -> Relation.T.(tyrel %<< bind vid int_)) in
      match exp with
      | Exp.Let_ (vid, exp1, exp2) ->
          let (tyenv, tyrel, ty) = infer tyenv tyrel exp1 in
          infer Env.T.(tyenv %<< (vid, ty)) tyrel exp2
      | Exp.Branch (cs) ->
          let rtn_ty = gen_var ~suffix:"-branch" () in
          let (tyenv, tyrel, types) =
            List.fold cs ~init:(tyenv, tyrel, [rtn_ty]) ~f:(fun (tyenv, tyrel, types) (cond, exp) ->
              let (tyenv, tyrel, ty) = infer tyenv tyrel exp in
              let tyrel = bind_condition_variables tyrel cond in
              (tyenv, tyrel, ty :: types)
            ) in
          (tyenv, Relation.T.(tyrel %<< types), rtn_ty)
      | Exp.App (fid, aid) ->
          let arg_type = gen_var ~suffix:("-app-arg-" ^ fid ^ "-" ^ aid) () in
          let rtn_type = gen_var ~suffix:("-app-rtn-" ^ fid ^ "-" ^ aid) () in
          let tyrel = Relation.T.(tyrel %<< bind aid arg_type %<< bind fid (func arg_type rtn_type)) in
          (tyenv, tyrel, rtn_type)
      | Exp.Term fml when Cond.is_var fml ->
          (tyenv, tyrel, Env.find_exn tyenv (Cond.vid_exn fml))
      | Exp.Term fml ->
          (tyenv, bind_condition_variables tyrel fml, int_)
      | Exp.Fail -> (tyenv, tyrel, Top) in
    let init_tyenv =
      List.fold fs ~init:Env.empty ~f:(
        fun tyenv ({ Func.name; Func.args; _ } : Func.t) ->
        let arg_types = List.map args ~f:(fun aid -> gen_var ~suffix:("-fun-arg-" ^ name ^ "-" ^ aid) ()) in
        let rtn_type = gen_var ~suffix:("-fun-rtn-" ^ name) () in
        let fun_type = multiple_func arg_types rtn_type in
        Env.T.(List.fold (List.zip_exn args arg_types) ~f:Env.cons ~init:tyenv %<< (name, fun_type))
      )
    in
    let infer_each_func (tyenv, tyrel) (
      { Func.name ; Func.args ; Func.exp ; _ } : Func.t
    ) =
      let arg_types = List.map args ~f:(fun aid -> Env.find_exn tyenv aid) in
      let (tyenv, tyrel, rtn_type) = infer tyenv tyrel exp in
      let fun_type = multiple_func arg_types rtn_type in
      let tyrel = Relation.T.(tyrel %<< (Env.find_exn tyenv name == fun_type)) in
      (tyenv, tyrel) in
    let (tyenv, tyrel) = List.fold fs ~f:infer_each_func ~init:(init_tyenv, Relation.empty) in
    let tyenv = Relation.unify (tyenv (* |> Env.inspect *) ) (tyrel (* |> Relation.inspect *) ) in
    let fids = List.map fs ~f:(fun ({ Func.name ; _ } : Func.t) -> name) in
    Env.filteri tyenv ~f:(fun ~key ~data:_ -> List.mem fids key ~equal:(=))
end

let use_refinement_annotation = ref true

let reftype_env_of (self : t) : Type.Env.t =
  let tyenv = SimpleTypeInfer.main self in
  let env = Type.Env.from_simple_type_env tyenv in
  if !use_refinement_annotation then
    List.fold (recfuns self) ~init:env ~f:(fun env func ->
      match func |> Func.annotation |> Func.Annotation.reftype with
      | None -> env
      | Some reftype ->
        let key = Func.(func.name) in
        Type.Env.T.(env @<< from_map (key, Type.RefType.fresh reftype))
    )
  else
    env
