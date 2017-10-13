open Core

module Chunk = struct
  open Int63
  type t = Int63.t
  let empty = zero
  let get t i = bit_and t (shift_left one i) <> zero
  let set t i v =
    if v then bit_or t (shift_left one i)
    else bit_and t (bit_xor minus_one (shift_left one i))

  let mul (self : t) (another : t) = bit_and self another
  let rev (self : t) = bit_not self
  let nonzero (self : t) = self <> zero
  let equal (self : t) (another : t) = self = another

  let count_bit (self : t) : int =
    let (%&) x y = bit_and x y in
    let count32 (i : t) : t =
      let i = i - (shift_right_logical i 1 %& of_int 0x55555555) in
      let i = (i %& of_int 0x33333333) + (shift_right_logical i 2 %& of_int 0x33333333) in
      let i = (i + shift_right_logical i 4) %& of_int 0x0f0f0f0f in
      let i = i + shift_right_logical i 8 in
      let i = i + shift_right_logical i 16 in
      i %& of_int 0x3f
    in
    Option.value_exn (
      to_int (
        count32 (shift_right_logical self 32)
        + count32 (self %& of_int 0xffffffff)
      )
    )
end

type t = {
  mutable length: int;
  mutable data: Int63.t Array.t;
}
[@@deriving fields, sexp]

let chunk_length = 62
let chunk_index i = i / chunk_length
let index i = i mod chunk_length

let create length =
  Fields.create length (Array.create ~len:(1 + (length / chunk_length)) Int63.zero)

let get (self : t) i =
  Chunk.get self.data.(chunk_index i) (index i)

let set (self : t) i v =
  let chunk = (chunk_index i) in
  self.data.(chunk) <- Chunk.set self.data.(chunk) (index i) v

let mul (self : t) (another : t) : t =
  let len = if self.length > another.length then another.length else self.length in
  let bv = create len in
  let intrange = List.range 0 (chunk_index (len - 1) + 1) in
  let () = List.iter intrange ~f:(fun i ->
    bv.data.(i) <- Chunk.mul self.data.(i) another.data.(i)
  ) in
  bv

let rev (self : t) : t =
  let bv = create self.length in
  let intrange = List.range 0 (chunk_index (self.length - 1) + 1) in
  let () = List.iter intrange ~f:(fun i ->
    bv.data.(i) <- Chunk.rev self.data.(i)
  ) in
  bv

let nonzero (self : t) : bool =
  let max = chunk_index (self.length - 1) in
  let rec loop i =
    if i >= max then false
    else if Chunk.nonzero self.data.(i) then true
    else loop (i + 1)
  in loop 0

let equal (self : t) (another : t) : bool =
  if self.length <> another.length then false
  else
    let max = chunk_index (self.length - 1) in
    let rec loop i =
      if i >= max then false
      else if Chunk.equal self.data.(i) another.data.(i) then loop (i + 1)
      else loop (i + 1)
    in loop 0

let expand (self : t) amount =
  let append_chunk_size = chunk_index (self.length + amount) - chunk_index self.length in
  let () = self.length <- self.length + amount in
  if append_chunk_size > 0
  then self.data <- Array.append self.data (Array.create ~len:append_chunk_size Int63.zero)
  else ()

let foldi (self : t) ~init ~f =
  let intrange = List.range 0 self.length in
  List.fold intrange ~init ~f:(fun acc i ->
    f i acc (get self i)
  )

let iteri (self : t) ~f =
  let intrange = List.range 0 self.length in
  List.iteri intrange ~f:(fun acc i ->
    f i (get self i)
  )

let count_bit (self : t) : int =
  let intrange = List.range 0 (chunk_index (self.length - 1) + 1) in
  List.fold intrange ~init:0 ~f:(fun acc i ->
    acc + Chunk.count_bit self.data.(i)
  )

let rec shift (self : t) amount : t =
  let el = create (self.length + amount) in
  let () = iteri self ~f:(fun i v ->
    set el (i + amount) v
  ) in
  el

let bor (self : t) (another : t) : t =
  let len = if self.length > another.length then self.length else another.length in
  let el = create len in
  let sclen = chunk_index self.length in
  let aclen = chunk_index another.length in
  let new_data = Array.mapi el.data ~f:(fun i _ ->
    let sd = if i > sclen then Int63.zero else self.data.(i) in
    let ad = if i > aclen then Int63.zero else another.data.(i) in
    Int63.bit_or sd ad
  ) in
  { length = len; data = new_data; }

let fold (self : t) ~init ~f =
  let rec loop acc i =
    if i < self.length
    then loop (f acc (get self i)) (i + 1)
    else acc
  in loop init 0

let sexp_of_t self =
  Array.sexp_of_t Bool.sexp_of_t
    (Array.init self.length ~f:(fun i -> get self i))

let t_of_sexp sexp =
  let a = Array.t_of_sexp Bool.t_of_sexp sexp in
  let t = create (Array.length a) in
  Array.iteri a ~f:(fun i v -> set t i v);
  t

let hash self : int = Int63.to_int_exn (Int63.rem (Array.get self.data 0) (Int63.of_int Int.max_value))
let compare self other =
  if self.length <> other.length
  then Int.compare self.length other.length
  else List.fold (List.range 0 (1 + chunk_index self.length) |> List.rev) ~init:0 ~f:(fun acc i ->
      if acc = 0 then Int63.compare (Array.get self.data i) (Array.get other.data i) else acc
    )
