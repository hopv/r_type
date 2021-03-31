open Core
open SugarProgram
open Util

module L = Label.Make (struct
  let label = "innerFunc"
end)

module BindClosure = struct
  let main (sprogram : t) : t =
    let mapper =
      { Mapper.default_mapper with
        Mapper.let_ = (fun self (vid, exp1, exp2) ->
          let exp1 =
            match exp1 with
            | AbsExp (vids, exp) -> AbsExp (vids, self.Mapper.exp self exp)
            | _ -> self.Mapper.exp self exp1
          in
          LetExp (vid, exp1, self.Mapper.exp self exp2)
        );
        Mapper.letrec_ = (fun self (bindings, exp2) ->
          let bindings = List.map bindings ~f:(fun (v, exp) ->
            let exp =
              match exp with
              | AbsExp (vids, exp) -> AbsExp (vids, self.Mapper.exp self exp)
              | _ -> self.Mapper.exp self exp2
            in
            (v, exp)
          ) in
          LetRecExp (bindings, self.Mapper.exp self exp2)
        );
        Mapper.abs = (fun self (vids, exp) ->
          let v = L.gen () in
          LetExp (v, AbsExp (vids, self.Mapper.exp self exp), ObjExp (Objt.mk_var v))
        );
      }
    in
    SugarProgram.expr_map sprogram ~f:(mapper.Mapper.exp mapper)
end

module InnerFunc = struct
  type t = Identity.t * exp * Identity.t list * Identity.t list

  let name_of ((name, _, _, _) : t) = name
  let exp_of ((_, exp, _, _) : t) = exp
  let extra_vids_of ((_, _, vids, _) : t) = vids
  let abs_vids_of ((_, _, _, vids) : t) = vids

  let to_func (self : t) : recfun =
    { name = (name_of self); args = (extra_vids_of self @ abs_vids_of self); exp = (exp_of self); annotation = Program.Func.Annotation.make (); }
end

module Env = struct
  module Element = struct
    type t = (Identity.t * Identity.t list)
  end

  type t = Element.t Identity.Map.t

  let empty = Identity.Map.empty
  let add (self : t) ~key ~data =
    match Identity.Map.add self ~key ~data with
    | `Ok self -> self
    | `Duplicate -> self
  let add_closure self ~key ~data = add self ~key ~data:(data)
  let add_global_function self ~key ~data = add self ~key ~data:(data, [])

  let global_function_key_set_of (self : t) =
    Identity.Map.filteri self ~f:(fun ~key:_ ~data ->
      match data with
      | (_, []) -> true
      | _ -> false
    ) |> Identity.Map.keys |> Identity.Set.of_list

  let keys (self : t) =
    self |> Identity.Map.keys

  let key_set_of (self : t) =
    Identity.Set.of_list (keys self)

  let find = Identity.Map.find
end

let main (sprogram : t) : t =
  let inner_funcs : (InnerFunc.t list) ref = ref [] in
  let mapping_expr (mapper : Mapper.t) (expr : Expr.t) : Expr.t = Mapper.apply_expr mapper expr in
  let sort_vars (var_set : Identity.Set.t) (vars : Identity.t list) = List.filter vars ~f:(Identity.Set.mem var_set) in
  let rec mapper_fn (env : Env.t) (vars_rev : Identity.t list) : Mapper.t =
    { Mapper.default_mapper with
      Mapper.let_ = (fun self (vid, exp1, exp2) ->
        match self.Mapper.exp self exp1 with
        | (AbsExp (args, abs_exp)) ->
          let env_keys = Env.key_set_of env in
          let (fname, free_vars) as closure = (L.gen () ^ "-" ^ vid, sort_vars (Identity.Set.diff (Identity.Set.of_list vars_rev) (env_keys)) (List.rev vars_rev)) in
          let inner_func : InnerFunc.t = (fname, abs_exp, free_vars, args) in
          let () = inner_funcs := inner_func :: !inner_funcs in
          mapping_expr (mapper_fn (Env.add_closure env ~key:vid ~data:closure) (vid :: vars_rev)) exp2
        | x -> LetExp (vid, x, mapping_expr (mapper_fn env (vid :: vars_rev)) exp2)
      );
      Mapper.letrec_ = (fun _self (bindings, exp2) ->
        let env_keys = Env.key_set_of env in
        let letrec_bound_names = List.map bindings ~f:Tuple2.get1 in
        let letrec_bound_name_set = Identity.Set.of_list letrec_bound_names in
        let env =
          let vid_closures = List.map bindings ~f:(fun (vid, _exp) ->
            let free_vars = Identity.Set.diff (Identity.Set.diff (Identity.Set.of_list vars_rev) (env_keys)) letrec_bound_name_set in
            let closure = (L.gen () ^ "-" ^ vid, sort_vars free_vars (List.rev vars_rev)) in
            (vid, closure)
          ) in
          List.fold vid_closures ~init:env ~f:(fun env (key, data) -> Env.add_closure env ~key ~data)
        in
        let mapper = mapper_fn env (letrec_bound_names @ vars_rev) in
        let bindings = List.map bindings ~f:(fun (vid, exp) ->
          match mapping_expr mapper exp with
          | (AbsExp (args, abs_exp)) ->
            let (fname, free_vars) = Option.value_exn (Env.find env vid) in
            let inner_func : InnerFunc.t = (fname, abs_exp, free_vars, args) in
            let () = inner_funcs := inner_func :: !inner_funcs in
            None
          | x -> Some (vid, x)
        ) |> List.filter_opt in
        List.fold bindings ~init:(Mapper.apply_expr mapper exp2) ~f:(fun cont (vid, exp) -> LetExp (vid, exp, cont))
      );
      Mapper.abs = (fun _self (vids, exp) ->
        AbsExp (vids, mapping_expr (mapper_fn env (List.rev vids @ vars_rev)) exp)
      );
      Mapper.obj = (fun _self objt ->
        match (Objt.vid_of objt |> (fun x -> Option.bind x ~f:(Env.find env))) with
        | None -> ObjExp objt
        | Some (fname, []) -> ObjExp (Objt.mk_var fname)
        | Some (fname, free_vars) -> FuncCallExp (fname, List.map free_vars ~f:(fun x -> ObjExp (Objt.mk_var x)))
      );
      Mapper.funccall = (fun self (fid, aes) ->
        let aes = List.map aes ~f:(self.Mapper.exp self) in
        match Env.find env fid with
        | None -> FuncCallExp (fid, aes)
        | Some (fname, []) -> FuncCallExp (fname, aes)
        | Some (fname, free_vars) -> FuncCallExp (fname, List.map free_vars ~f:(fun x -> ObjExp (Objt.mk_var x)) @ aes)
      );
    }
  in
  let initial_env = List.fold (SugarProgram.recfuns_of sprogram) ~init:Env.empty ~f:(fun env recfun ->
    Env.add_global_function env ~key:(Func.name_of recfun) ~data:(Func.name_of recfun)
  ) in
  let env_keys = Env.keys initial_env in
  let sprogram = BindClosure.main sprogram in
  let sprogram = SugarProgram.map sprogram ~f:(fun el ->
    match el with
    | BindFunc recfun ->
      let exp = mapping_expr (
        mapper_fn initial_env (
          List.rev recfun.SugarProgram.args @ env_keys
        )
      ) recfun.SugarProgram.exp in
      BindFunc { recfun with exp = exp; }
    | _ -> StructureItem.expr_map el ~f:(mapping_expr (mapper_fn initial_env env_keys))
  ) in
  sprogram @ List.map ~f:(SugarProgram.StructureItem.bind_func @< InnerFunc.to_func) !inner_funcs
