open Core

module type S = sig
  type t
  val dig : t -> (t -> t) -> t
end

module Make = functor(M : S) -> struct
  let dig = M.dig
  let rec recdig e f = f (M.dig e (fun e -> recdig e f))
end
