open Core

let cnt = ref 0

let alpha_vid id vid =
  "$alpha-" ^ string_of_int id ^ ":" ^ vid

let freevar_vid vid =
  "$freevar:" ^ vid

let binop_vid vid = "$binop:" ^ string_of_int vid

let tmp_vid vid = "$tmp:" ^ string_of_int vid

let strip_wrapper vid =
  Re2.replace
    (Re2.create_exn "\\$[^:]*:") ~f:(fun _ -> "") vid

let shorten vid =
  Re2.replace
    (Re2.create_exn "\\$(.)[^:]*:") ~f:(fun t -> "$" ^ Re2.Match.get_exn t ~sub:(`Index 1)) vid

module type Context = sig
  val label : string
end

module Make (C : Context) = struct
  let cnt = ref 0
  let gen ?(prefix = "") () = cnt := !cnt + 1; prefix ^ "$" ^ C.label ^ ":" ^ string_of_int !cnt
end
