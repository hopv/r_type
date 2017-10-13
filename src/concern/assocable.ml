
open Core

module type S = sig
  type t
  type key_t
  type value_t
  val mapping : t -> (key_t * value_t) list
end

module Make = functor(M : S) -> struct
  let assoc obj key =
    List.Assoc.find (M.mapping obj) key

  let assoc_exn obj key =
    List.Assoc.find_exn (M.mapping obj) key
end
