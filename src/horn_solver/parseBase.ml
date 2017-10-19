open Lib

(** A list of variable and type pairs. *)
type args = (string * string) list

(** An ADT representing a predicate definition's body. *)
type body =
(** A existential quantifier. *)
| Qtf of args * body
(** Operator application. *)
| App of string * body list
(** Predicate application. *)
| PApp of string * body list
(** A let-binding. *)
| Let of (string * body) list * body
(** Variable or constant. *)
| Leaf of string


let needs_parens = function
| Leaf _ -> false
| _ -> true

(** Pretty printer for arguments. *)
let fmt_args fmt (args: args) =
  Core.List.iter args ~f:(
    fun (id, _) ->
      Format.fprintf fmt " %s" id
  )

(** Pretty printer for body, good old ugly recursion. *)
let rec fmt_body fmt: body -> unit = function
| Qtf (args, body) ->
  Format.fprintf fmt "Exists (%a). %a" fmt_args args fmt_body body
| App ("and", arg :: []) -> Format.fprintf fmt "%a" fmt_body arg
| App ("or", arg :: []) -> Format.fprintf fmt "%a" fmt_body arg
| App (op, arg :: []) ->
  if needs_parens arg then
    Format.fprintf fmt "(%s (%a))" op fmt_body arg
  else
    Format.fprintf fmt "(%s %a)" op fmt_body arg
| App ("ite", c :: t :: e :: []) ->
  Format.fprintf fmt "if %a then %a else %a" fmt_body c fmt_body t fmt_body e
| App (op, args) -> Core.List.fold_left args ~init:true ~f:(
  fun is_first arg ->
    let parens = needs_parens arg in
    ( if is_first then (
        if parens then
          Format.fprintf fmt "(@   @[<v>%a@]@ )" fmt_body arg
        else
          Format.fprintf fmt "%a" fmt_body arg
      ) else (
        if parens then
          Format.fprintf fmt " %s (@   @[<v>%a@]@ )" op fmt_body arg
        else
          Format.fprintf fmt " %s %a" op fmt_body arg
      )
    ) ;
    false
) |> ignore
| PApp (pred, args) ->
  Format.fprintf fmt "@[<v>(%s" pred ;
  Core.List.iter args ~f:(
    fun arg ->
      let (opn, cls) = if needs_parens arg then ("(", ")") else ("", "") in
      Format.fprintf fmt "@   @[<v>%s%a%s@]" opn fmt_body arg cls
  ) ;
  Format.fprintf fmt "@ )@]"
| Let ((id, expr) :: [], body) ->
  Format.fprintf fmt "let %s =@   @[<v>%a@]@ in@ %a" id fmt_body expr fmt_body body
| Let (bindings, body) ->
  Format.fprintf fmt "@[<v>let (@ " ;
  Core.List.fold_left bindings ~init:true ~f:(
    fun is_first (id, _) ->
      ( if is_first then
        Format.fprintf fmt "  %s" id
      else
        Format.fprintf fmt ",@   %s" id
      ) ;
      false
  ) |> ignore ;
  Format.fprintf fmt "@ ) = (@ " ;
  Core.List.fold_left bindings ~init:true ~f:(
    fun is_first (_, expr) ->
      ( if is_first then
        Format.fprintf fmt "  %a" fmt_body expr
      else
        Format.fprintf fmt ",@   %a" fmt_body expr
      ) ;
      false
  ) |> ignore ;
  Format.fprintf fmt "@ ) in@ @]"
| Leaf s -> Format.fprintf fmt "%s" s

(** A model maps predicate identifiers to optional definitions. *)
type model = (string * (args * body)) list

(** Pretty printer for a predicate definition. *)
let fmt_def fmt (name, (args, body)) =
  Format.fprintf fmt "let %s%a =@   @[<v>%a@]"
    name fmt_args args fmt_body body

(** Result of parsing the solver's output. *)
type parse =
| Sat
| Unsat
| Error of string
| Model of model
| None

(** String description of a parse result. *)
let desc_of = function
| Sat -> "sat"
| Unsat -> "unsat"
| Error _ -> "error"
| Model _ -> "model"
| None -> "nothing"