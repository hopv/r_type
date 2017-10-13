
open Core

module V = Label.Make(struct let label = "unknown" end)

type id = Identity.t
[@@deriving eq, sexp, ord, hash]

module T = struct
  type t =
    { id: Identity.t; vars: Identity.t list; [@no_hashing] var_set: Identity.Set.t [@no_hashing] }
  [@@deriving compare, sexp, hash]

  let equal (self : t) (another : t) : bool = Identity.equal self.id another.id
  let compare (self : t) (another : t) : int = Identity.compare self.id another.id
  let to_string self =
    Identity.Short.show self.id ^ List.to_string self.vars ~f:Identity.Short.show
end
include T
include Comparable.Make(T)
include Hashable.Make(T)

let init ?prefix variables : t =
  { id = V.gen ?prefix (); vars = variables; var_set = Identity.Set.of_list variables }

(* let subst self orig_var new_var =
  if List.mem self.vars orig_var then
    { self with var_subst = Subst.add self.var_subst (orig_var, new_var) }
  else
    { self with var_subst = Subst.subst self.var_subst orig_var new_var } *)

(* let subst_of self = self.var_subst *)
let get_vids self = self.vars
let vids_of self = self.vars
let var_set_of self = self.var_set
(* let substed_vids_of self =
  List.map self.vars ~f:(fun v ->
    Option.value (Subst.find self.var_subst v) ~default:v
    ) *)

let id_of (self : t) = self.id

let sort_vars = List.sort ~cmp:(Identity.compare)

(* let get_obj_substs (self : t) =
  List.fold (List.map self.vars ~f:(fun x -> (x, x)) @ self.var_subst) ~init:ObjSubst.Map.empty ~f:(fun acc (k, v) ->
    match ObjSubst.Map.find self.obj_subst v with
    | Some x -> ObjSubst.Map.add acc ~key:k ~data:x
    | None -> acc
  ) *)

let id_of self = self.id
let short_id_of self = Identity.Short.show self.id
let compare self other = Identity.compare (id_of self) (id_of other)

let show_id = short_id_of

let mem self vid = Identity.Set.mem self.var_set vid

module Testability = struct
  let create name : t = { id = name; vars = []; var_set = Identity.Set.empty; }
end
