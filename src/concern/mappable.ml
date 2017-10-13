
open Core

module type S = sig
  type t [@@deriving eq, ord, sexp, hash]
end

module Make = functor(M : S) -> struct
  include M
  include Comparable.Make(M)
  include Hashable.Make(M)
end
