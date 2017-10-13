open Core
open Program

module Defs = struct
  module Map = Identity.Map
  type closure_t = {
    name: Identity.t;
    exp: Exp.t;
    vars: Identity.t list;
    ty: Type.RefType.t;
    mutable env: env_t;
  }
  [@@deriving fields, sexp, eq, ord]
  and value_t = Fail | Basic of Objt.t | Clos of closure_t
  [@@deriving sexp, eq, ord]
  and env_t = value_t Map.t
  [@@deriving sexp, eq, ord]
end

module Env = struct
  include Defs.Map
  type tt = Defs.env_t
end

module Value = struct
  module T = struct
    type t = Defs.value_t
    [@@deriving sexp, eq, ord]
  end
  include T
  include Showsexp.Make(T)

  let is_fail (self : t) = match self with Defs.Fail -> true | _ -> false
  let is_safe (self : t) = not (is_fail self)

  let obj_of (self : t) = match self with Defs.Basic obj -> Some obj | _ -> None
  let obj_value (obj : Objt.t) : t = Defs.Basic obj
  let pure cond = Defs.Basic cond

  module Closure = struct
    type t = Defs.closure_t
    [@@deriving sexp, eq, ord]

    let is_satisfied (self : t) = List.is_empty self.Defs.vars

(*     let assign (self : t) (v : Defs.value_t) : t =
      let (x, xs) = (List.hd_exn self.Defs.vars, List.tl_exn self.Defs.vars) in
      let (arg_ty, rtn_ty) = (
        Type.RefType.arg self.Defs.ty, Type.RefType.rtn self.Defs.ty
      ) in
      let (arg_ty, rtn_ty) =
        (match obj_of v with
          | Some obj -> (
            Type.RefType.assign_obj arg_ty (Type.RefType.name_of arg_ty) obj,
            Type.RefType.assign_obj rtn_ty (
              Type.RefType.name_of self.Defs.ty
            ) obj
          )
          | None -> (arg_ty, rtn_ty) )in
      let examples = ([get_unknown arg_ty] |> List.filter_opt) @ self.examples
      in { self with vars = xs; ty = rtn_ty; env = Env.add self.env ~key:x ~data:v; examples = examples } *)

    let random_input (self : t) =
      let arg_types = Type.RefType.argument_types self.Defs.ty in
      List.map arg_types ~f:(fun ty ->
          if Type.RefType.is_base ty then
            Objt.mk_int (Random.int 20) |> obj_value
          else failwith "unsupported"
        )

    let func_to_closure (func : Func.t) (ty : Type.RefType.t) (env : Env.tt) : t = {
        Defs.name = func.Func.name ;
        Defs.exp = func.Func.exp ;
        Defs.vars = func.Func.args ;
        Defs.ty = ty ;
        Defs.env = env ;
      }

    let to_string (self : t) : string =
      "{ name: " ^ self.Defs.name ^ ";\n" ^
      "  ty: " ^ Type.RefType.to_string self.Defs.ty ^ "; }"
  end

  let to_string (self : t) : string =
    match self with
    | Defs.Fail -> "fail"
    | Defs.Basic (v) -> Objt.to_string v
    | Defs.Clos (v) -> Closure.to_string v

  let closure_value (c : Closure.t) : t = Defs.Clos c
  let closure_of (self : t) : Closure.t option = match self with Defs.Clos c -> Some c | _ -> None
end
