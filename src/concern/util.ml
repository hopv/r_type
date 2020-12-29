
open Core

module Util = struct
  let rec join (sep : string) (strings : string list) =
    match strings with
    | [] -> ""
    | [str] -> str
    | x :: xs -> x ^ sep ^ join sep xs

  let rec join_with strs ~separator =
    join separator strs

  let insert_indent indent str =
    let rec spaces indent =
      if indent > 0 then "  " ^ spaces (indent - 1) else ""
    in
    let hd = spaces indent in
    String.split_lines str |> List.map ~f:(fun x -> hd ^ x) |> join_with ~separator:"\n"

  let rec wrap str = "(" ^ str ^ ")"

  let some_break (self : 'a option) ~f : 'a =
    match self with
    | Some x -> x
    | None -> f ()

  let shuffle (xs : 'a list) : 'a list =
    let ys = List.map xs ~f:(fun x -> (Random.float 1., x)) in
    let ys' = List.sort ~compare:(fun (score, x) (score', y) -> Float.compare score score') ys in
    List.map ys' ~f:(fun (_, x) -> x)

  let random (xs : 'a list) : ('a * 'a list) option =
    let len = List.length xs in
    if len <= 0 then
      None
    else
      let pick_idx = Random.int len in
      List.foldi xs ~init:(List.hd_exn xs, []) ~f:(fun idx (picked, others) x ->
        if idx = pick_idx then
          (x, others)
        else
          (picked, x :: others)
      ) |> (fun (picked, xs') -> (picked, List.rev xs')) |> Option.some

  let (@<) g f = fun x -> g (f x)
  let (<|) f x = f @@ x
end
include Util
