(** Helper types and functions. *)


(** Result type to make caml slightly less unsafe. *)
module Res = struct

  (** Ok or an error. *)
  type ('ok, 'err) res =
  | Ok of 'ok
  | Err of 'err

  (** Map over `Ok`. *)
  let map f = function
  | Ok res -> Ok (f res)
  | Err e -> Err e

  (** Map over `err`. *)
  let map_err f = function
  | Err e -> Err (f e)
  | ok -> ok

  (** Chains an error. *)
  let chain_err e = function
  | Err err -> Err (e :: err)
  | ok -> ok

  (** Creates an `Ok`. *)
  let ok something = Ok something
  (** True if variant is `Ok`. *)
  let is_ok = function | Ok _ -> true | _ -> false
  (** True if variant is `Err`. *)
  let is_err = function | Err _ -> true | _ -> false
  (** Creates an `Err`. *)
  let err something = Err something

  (** Does something if not an error. *)
  let and_then (
    work: 'a -> ('b, 'err) res
  ): ('a, 'err) res -> ('b, 'err) res = function
  | Ok res -> work res
  | Err err -> Err err

  (** Return the value in `Ok` or prints an error and exits. *)
  let unwrap blah = function
  | Ok res -> res
  | Err e ->
    Format.fprintf Format.err_formatter "Error %s:" blah ;
    List.iter (Format.fprintf Format.err_formatter "@.  %s") e ;
    Format.fprintf Format.err_formatter "@." ;
    exit 2

end



(** Wraps unsafe code in a result. *)
let sanitize (blah: string) (f: unit -> 'a): ('a, string list) Res.res =
  try f () |> Res.ok with e -> [
    blah ; Printexc.to_string e
  ] |> Res.err