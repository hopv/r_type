
open Core
open Sexplib.Std

type id = Identity.t
[@@deriving eq, ord, sexp, hash]

module Typedef = struct
  type t =
    | VarObj of id
    | SpecialVar of id
    | IntObj of int
    | BoolObj of bool
    | Array of int list
  [@@deriving eq, ord, sexp, variants, hash]

  let to_format obj =
    Formatable.text (
      match obj with
      | VarObj vid -> Identity.Short.show vid
      | SpecialVar vid -> vid
      | IntObj i -> string_of_int i
      | BoolObj i -> string_of_bool i
      | _ -> failwith "unexpected"
    )
end
include Typedef
include Showsexp.Make(Typedef)
include Formatable.Make(Typedef)

type typing = IntType | BoolType

let return_vid = "#return"
let return_vid_of fid = return_vid (* ("#return-" ^ fid) *)
let return_vid_main = return_vid (* "#main-return" *)

let wrap_to_string vid = Label.strip_wrapper vid

let mk_var (vid : id) = VarObj vid
let mk_int n = IntObj n

let mk_nu = SpecialVar "V"
let mk_program_variable = SpecialVar "X"

let get_vids = function
  | VarObj vid -> [vid]
  | _ -> []

let vids_of = get_vids

let true_ = BoolObj true
let false_ = BoolObj false
let mk_bool n = if n then true_ else false_

let is_truthy = function
  | VarObj _ -> false
  | IntObj i -> i <> 0
  | BoolObj b -> b
  | _ -> failwith "unexpected"

let is_falsey = function
  | VarObj _ -> false
  | IntObj i -> i = 0
  | BoolObj b -> not b
  | _ -> failwith "unexpected"

let is_bool = function
  | BoolObj _ -> true
  | _ -> false

let get_bool obj =
  if is_truthy obj
        then Some true
        else if is_falsey obj
             then Some false
             else None

let get_int = function
  | VarObj _ -> None
  | IntObj i -> Some i
  | BoolObj b -> Some (if b then 1 else 0)
  | _ -> failwith "unexpected"

let is_var = function
  | VarObj _ -> true
  | _ -> false

let int_of = function
  | IntObj n -> n
  | x -> failwith ("unexpected" ^ to_string x)

let vid_of = function
  | VarObj vid -> Some vid
  | _ -> None

let vid_of_exn = function
  | VarObj vid -> vid
  | _ -> failwith "unexpected"

let does_match_vid obj k =
  match obj with
  | VarObj vid -> if vid = k then true else false
  | SpecialVar vid -> if vid = k then true else false
  | _ -> false

let subst obj (k, v) =
  if does_match_vid obj k then v else obj

let subst_obj obj (k, v) =
  if equal obj k then v else obj

let string_of = to_string

let orig_string_of obj = Label.strip_wrapper (string_of obj)

let string_of_typing = function
  | IntType -> "int"
  | BoolType -> "bool"

let type_of _ = IntType

let is_int = function IntObj i -> true | _ -> false

(* let vid_set vid_list = List.sort_uniq String.compare vid_list *)

module VarSet = Set.Make(String)


let get_fv t =
  match t with
  | VarObj x -> [x]
  | SpecialVar _ -> assert false
  | IntObj _
  | BoolObj _
  | Array _ -> []
