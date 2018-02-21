
type t =
    Plus
  | Minus
  | Times
  | Div
  | Mod
  | Eq
  | Neq
  | Leq
  | Lt
  | Geq
  | Gt
  | Not_
  | And_
  | Or_
  | Iff
  | Impl
  | BtoI
  | ItoB
  | Forall of Identity.t
  | Exists of Identity.t
[@@deriving eq, ord, sexp, hash]






let string_of = function
    Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Mod -> "mod"
  | Eq -> "="
  | Neq -> "<>"
  | Leq -> "<="
  | Lt -> "<"
  | Geq -> ">="
  | Gt -> ">"
  | Not_ -> "!"
  | And_ -> "&&"
  | Or_ -> "||"
  | Iff -> "<=>"
  | Impl -> "==>"
  | BtoI -> "btoi"
  | ItoB -> "itob"
  | Forall v -> "Forall(" ^ Identity.Short.show v ^ ")"
  | Exists v -> "Exists(" ^ Identity.Short.show v ^ ")"

let of_string = function
  | "+" -> Some Plus
  | "~-" -> Some Minus
  | "-" -> Some Minus
  | "/" -> Some Div
  | "*" -> Some Times
  | "mod" -> Some Mod
  | "=" -> Some Eq
  | "<>" -> Some Neq
  | "<=" -> Some Leq
  | "<" -> Some Lt
  | ">" -> Some Gt
  | ">=" -> Some Geq
  | "!" -> Some Not_
  | "&&" -> Some And_
  | "||" -> Some Or_
  | "<=>" -> Some Iff
  | "==>" -> Some Impl
  | "not" -> Some Not_
  | _ -> None

let is_value_bool = function
  | Eq -> true
  | Neq -> true
  | Leq -> true
  | Lt -> true
  | Geq -> true
  | Gt -> true
  | Not_ -> true
  | And_ -> true
  | Or_ -> true
  | Iff -> true
  | Impl -> true
  | ItoB -> true
  | Forall _ -> true
  | Exists _ -> true
  | _ -> false

let is_arg_bool = function
  | Not_ -> true
  | And_ -> true
  | Or_ -> true
  | Iff -> true
  | Impl -> true
  | BtoI -> true
  | Forall _ -> true
  | Exists _ -> true
  | _ -> false

let is_arg_polymorphic = function
  | Eq -> true
  | _ -> false

let is_quantifier = function
  | Forall _ -> true
  | Exists _ -> true
  | _ -> false

let type_of op =
  let bool_to_op bl = if bl then Objt.BoolType else Objt.IntType in
  (is_arg_bool op |> bool_to_op, is_value_bool op |> bool_to_op)

let equal_quantifier (self : t) (another : t) : bool =
  match self with
  | Forall _ -> (match another with | Forall _ -> true | _ -> false)
  | Exists _ -> (match another with | Exists _ -> true | _ -> false)
  | _ -> false

let forall v = Forall v
let exists v = Exists v

let quantifier_label = function
  | Forall _ -> "Forall"
  | Exists _ -> "Exists"
  | _ -> ""



module ToSmt2 = struct

  let op_strings op = match op with
  | Plus -> [ "+" ]
  | Minus -> [ "-" ]
  | Times -> [ "*" ]
  | Div -> [ "div" ]
  | Mod -> [ "mod" ]
  | Eq -> [ "=" ]
  | Neq -> [ "not" ; "=" ]
  | Leq -> [ "<=" ]
  | Lt -> [ "<" ]
  | Geq -> [ ">=" ]
  | Gt -> [ ">" ]
  | Not_ -> [ "not" ]
  | And_ -> [ "and" ]
  | Or_ -> [ "or" ]
  | Iff -> [ "=" ]
  | Impl -> [ "=>" ]
  | ItoB -> [ "not" ; "= 0" ]
  | op -> string_of op |> Format.sprintf "unsupported operator %s" |> failwith

end
