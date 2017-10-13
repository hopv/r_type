
open Core
module type S = sig
  type t

  val mk_or : t -> t -> t
  val mk_and : t -> t -> t
  val mk_false : t
  val mk_true  : t
end


module Make (M : S) = struct
  let forall group func =
    match group with
    | [] -> M.mk_true
    | hd :: tails ->
      List.fold_left
        ~f:(fun left_exp group_el -> M.mk_and left_exp (func group_el))
        ~init:(func hd)
        tails

  let exists group func =
    match group with
    | [] -> M.mk_false
    | hd :: tails ->
      List.fold_left
        ~f:(fun left_exp group_el -> M.mk_or left_exp (func group_el))
        ~init:(func hd)
        tails
end
