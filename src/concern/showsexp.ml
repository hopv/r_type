
module type S = sig
  type t

  val sexp_of_t : t -> Sexplib.Sexp.t
end

module Make (M : S) = struct
  let show_sexp (v : M.t) = M.sexp_of_t v |> Sexplib.Sexp.to_string
end
