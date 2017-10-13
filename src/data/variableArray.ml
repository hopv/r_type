
open Core
type 'a t = {
  mutable els: ('a option) Array.t;
  mutable content_length: int;
}

let chunk_length = 200

let create length =
  { els = Array.create ~len:chunk_length None; content_length = 0; }

let get (self : 'a t) (idx : int) : 'a option =
  if idx >= self.content_length then None
  else Array.get self.els idx

let expand_array (self : 'a t) (amount : int) : unit =
  let len = ((amount % chunk_length) + 1) * chunk_length in
  self.els <- Array.append self.els (Array.create ~len None)

let set (self : 'a t) (idx : int) (el : 'a) : unit =
  let expands = idx - (Array.length self.els - 1) in
  let () = if expands > 0 then expand_array self expands else () in
  let () = if idx >= self.content_length then self.content_length <- idx + 1 else () in
  Array.set self.els idx (Some el)


let iter (self : 'a t) ~f : unit =
  let rec loop (idx : int) : unit =
    if idx >= self.content_length then ()
    else
      match get self idx with
      | None -> loop (idx + 1)
      | Some x -> let () = f x in loop (idx + 1)
  in loop 0

let iter2 (self : 'a t) (xs : 'b list) ~f : unit =
  let rec loop (idx : int) (ys : 'b list) : unit =
    if idx >= self.content_length then ()
    else
      match get self idx with
      | None -> loop (idx + 1) ys
      | Some x ->
        match ys with
        | [] -> ()
        | y :: ys' -> let () = f x y in loop (idx + 1) ys'
  in loop 0 xs

let fold (self : 'a t) ~init ~f =
  let rec loop (idx : int) (acc : 'b) : 'b =
    if idx >= self.content_length then acc
    else
      match get self idx with
      | None -> loop (idx + 1) acc
      | Some x -> loop (idx + 1) (f acc x)
  in loop 0 init

let to_alist (self : 'a t) : (int * 'a) list =
  let rec loop (idx : int) (acc : (int * 'a) list) : (int * 'a) list =
    if idx < 0 then acc
    else
      match get self idx with
      | None -> loop (idx - 1) acc
      | Some x -> loop (idx - 1) ((idx, x) :: acc)
  in loop self.content_length []
