open Core
open Sexplib.Std

module Essential = struct
  (* simple type with top type *)
  type t = Func of t * t | Int_ | Bool_ | Top | Var of Identity.t
  [@@deriving sexp, variants, eq, ord, hash]

  let multiple_func arg_types rtn_type =
    List.fold_right arg_types ~f:func ~init:rtn_type

  let is_simple = function Int_ | Bool_ | Top -> true | _ -> false

  let rec to_string (self : t) : string =
    match self with
    | Func (t1, t2) -> "(" ^ to_string t1 ^ " -> " ^ to_string t2 ^ ")"
    | Int_ -> "int"
    | Bool_ -> "bool"
    | Top -> "top"
    | Var vid -> vid

end
include Essential

module L = Label.Make(struct let label = "simple-type" end)
let gen_var ?(suffix = "") ?(prefix = "") () = Var (prefix ^ L.gen () ^ suffix)

module Env = struct
  (* mapping of program variables to simple types *)
  include Identity.Map
  type tt = Essential.t t

  let to_string el =
    "\n" ^ List.fold (to_alist el) ~init:"" ~f:(fun str (key, data) ->
      key ^ ": " ^ Essential.to_string data ^ "\n" ^ str)
  let cons x (key, data) =
    match add x ~key ~data with
    | `Ok x -> x
    | `Duplicate -> x
  let cons_int x key = cons x (key, Int_)
  module T = struct
    let (%<<) (x : tt) (key, data) =
      match add x ~key ~data with
      | `Ok x -> x
      | `Duplicate -> x
  end

  let find_exn (self : 'a t) (key : Identity.t) : 'a =
    try find_exn self key with
    | e -> failwith ("Error searching for " ^ key ^ " (" ^ (Exn.to_string e) ^ ")")
end

module Subst = struct
  (* mapping of type variables to simple types *)
  include Identity.Map
  type tt = Essential.t t

  let to_string el =
    "\n" ^ List.fold (to_alist el) ~init:"" ~f:(fun str (key, data) ->
      key ^ ": " ^ Essential.to_string data ^ "\n" ^ str)

  let find_or_fallback tysubst key =
    match find tysubst key with
    | Some ty -> ty
    | None -> Int_ (* fallback top type to int type *)

  let find_or_var tysubst key =
    match find tysubst key with
    | Some ty -> ty
    | None -> Var key

  let rec assoc tysubst ty =
    let find_and_dig key = match find tysubst key with
    | Some ty -> assoc tysubst ty
    | None -> Var key in
    match ty with
    | Var vid -> find_and_dig vid
    | Func (t1, t2) -> Func (assoc tysubst t1, assoc tysubst t2)
    | ty -> ty

  let add tysubst ~key ~data =
    match data with
    | Var vid when String.equal vid key -> tysubst
    | _ ->
        match add tysubst ~key ~data with
        | `Ok tysubst -> tysubst
        | `Duplicate -> assert false

  let resolve (tyenv : Env.tt) tysubst =
    let assoc_or_fallback tysubst ty =
      let rec fallback_var = function
      | Var _vid -> Int_
      | Func (t1, t2) ->
          Func (fallback_var t1, fallback_var t2)
      | x -> x in
      fallback_var (assoc tysubst ty) in
    Env.map tyenv ~f:(assoc_or_fallback (tysubst))

  module T = struct
    let (%<<) (x : tt) (key, data) = add x ~key ~data
  end
end

module Relation = struct
  type t = Essential.t list list
  [@@deriving sexp, eq, ord, hash]

  let empty = []
  let bind vid ty = [Var vid; ty]

  module T = struct
    let (==) x y = [x; y]

    let (%<<) (x : t) y = y :: x

    let var_ v = Var v
  end

  let to_string el =
    let show_rel tys = List.fold tys ~init:"{" ~f:(fun str ty -> str ^ Essential.to_string ty ^ "|") ^ "}" in
    "\n" ^ List.fold el ~init:"" ~f:(fun str tys -> show_rel tys ^ "\n" ^ str)

  let to_subst (tyrel : t) =
    let ignore_top elem = List.filter elem ~f:(function Top -> false | _ -> true) in
    let rec (%%) (tysubst : Subst.tt) (elem : Essential.t list) =
      match elem with
      | [] -> tysubst
      | _ :: [] -> tysubst
      | t1 :: t2 :: tys ->
          match (Subst.assoc tysubst t1, Subst.assoc tysubst t2) with
          | (t1, t2) when Essential.equal t1 t2 -> tysubst %% (t2 :: tys)
          | (Func (a1, a2), Func (b1, b2)) ->
              tysubst %% T.(a1 == b1) %% T.(a2 == b2) %% (Func (b1, b2) :: tys)
          | (Var tv, ty) ->
              Subst.T.(tysubst %<< (tv, ty)) %% (ty :: tys)
          | (ty, Var tv) ->
              Subst.T.(tysubst %<< (tv, ty)) %% (ty :: tys)
          | (t1, t2) ->
            failwith ("Unsatisfiable type constraints. (" ^ Essential.to_string t1 ^ " = " ^ Essential.to_string t2 ^ ")")
    in List.fold (tyrel) ~init:Subst.empty ~f:(fun tysubst elem -> tysubst %% (ignore_top elem))

  let unify (tyenv : Env.tt) (tyrel : t) =
    try Subst.resolve tyenv (to_subst tyrel) with
    | e -> raise e
end
