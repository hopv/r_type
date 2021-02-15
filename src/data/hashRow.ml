
open Core

module Make = functor(M : Hashtbl.Key) -> struct
  module H = Hashable.Make_and_derive_hash_fold_t(M)

  type t = {
    els: M.t list;
    rev: (M.t list) Lazy.t;
    table: int H.Table.t;
    length: int;
  }
  [@@deriving fields]

  let find (self : t) (cond : M.t) : int option = H.Table.find (self.table) cond

  let elements (self : t) : M.t list =
    Lazy.force_val self.rev

  let add (self : t) (qual : M.t) : t =
    match H.Table.add self.table ~key:qual ~data:self.length with
    | `Ok ->
      let new_els = qual :: self.els in
      Fields.create ~els:new_els ~rev:(Lazy.from_fun (fun () -> List.rev new_els)) ~table:self.table ~length:(self.length + 1)
    | _ -> self

  let mem (self : t) (el : M.t) : bool = H.Table.mem self.table el

  let add_multi (self : t) (quals : M.t list) : t = List.fold quals ~init:self ~f:add

  let create quals =
    let table = H.Table.create () in
    let empty = Fields.create ~els:[] ~rev:(Lazy.from_val []) ~table ~length:0 in
    add_multi empty quals

  (* Return rev elements from (old.length)-th to (self.length - 1)-th. Assume old is subset of self *)
  let new_elements (self : t) (old : t) : M.t list =
    let size = self.length - old.length in
    let rec take i xs acc =
      if i <= 0 then acc
      else
        match xs with
        | [] -> acc
        | x' :: xs' -> take (i - 1) xs' (x' :: acc)
    in
    take size self.els []

  let length (self : t) = self.length

  let to_alist (self : t) : (M.t * int) list =
    List.mapi (elements self) ~f:(fun i x -> (x, i))
end
