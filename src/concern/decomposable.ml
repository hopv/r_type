
open Core

module type S = sig
  type t
  type 'a decompose_t

  val decompose : t -> (t -> 'a) -> 'a decompose_t
  val reassemble : t decompose_t -> t
end

module type T = sig
  type t
  type 'a decompose_t
  val decompose : t -> (t -> 'a) -> 'a decompose_t
  val reassemble : t decompose_t -> t
  val dmap : t -> ('a decompose_t -> 'a) -> 'a
end

module Make (M : S) : (T with type 'a decompose_t = 'a M.decompose_t and type t := M.t) = struct
  include M
  let decompose = M.decompose
  let reassemble = M.reassemble
  let rec dmap e f = f (decompose e (fun e' -> dmap e' f))
end
