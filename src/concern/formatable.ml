open Core

type t = Text of string | Indent of t list | Inline of t list | Block of t list
type fmt = t list

let is_inline = function
  | Text _ -> true
  | Inline _ -> true
  | _ -> false

let rec sort blk =
  let mk_group blks =
    let sort_queue =
      function
        (sum, []) ->  sum
      | (sum, xs) ->  Block (List.rev xs) :: sum
    in let loop (sum, current) blk =
      if is_inline blk
      then (sum, blk :: current)
      else (blk :: sort_queue (sum, current), [])
    in List.fold_left blks ~f:loop ~init:([], []) |> sort_queue |> List.rev
  in match blk with
    | Inline blks ->
        let blks' = List.map blks ~f:sort in
        if List.for_all blks' ~f:is_inline
        then Inline blks'
        else Indent (mk_group blks')
    | Block blks -> Block (mk_group blks)
    | Indent blks -> Indent (mk_group blks)
    | _ -> blk

let to_string blk =
  let rec to_string' indent blk =
    let join ls = List.fold_left ls ~init:"" ~f:(fun sum l -> sum ^ l) in
    match blk with
      | Text str -> str
      | Inline blks ->
          List.map blks ~f:(to_string' indent) |> join
      | Block blks ->
          if List.for_all blks ~f:is_inline
          then Util.insert_indent indent (List.map blks ~f:(to_string' indent) |> join) ^ "\n"
          else List.map blks ~f:(to_string' indent) |> join
      | Indent blks ->
          if List.for_all blks ~f:is_inline
          then Util.insert_indent (indent + 1) (List.map blks ~f:(to_string' (indent + 1)) |> join) ^ "\n"
          else List.map blks ~f:(to_string' (indent + 1)) |> join
  in to_string' 0 blk

let indent blk = Indent [blk]
let block str = Block str
let to_block el = Block [el]
let inline str = Inline str
let text str = Text str
let line str = block [text str]
let noline str = inline [text str]

let rec joint (xs : 'a list) (y : 'a) : 'a list =
  match xs with
    [] -> []
  | hd :: [] ->
      [hd]
  | hd :: tl ->
      hd :: y :: joint tl y

type format_t = t
module type S = sig
  type t
  val to_format : t -> format_t
end
let s (type a) (to_format : a -> format_t) =
  (module struct
    type t = a
    let to_format = to_format
  end : S)

module Make (M : S) = struct
  let to_format tr = tr |> M.to_format |> sort
  let to_string tr = tr |> to_format |> to_string
end
