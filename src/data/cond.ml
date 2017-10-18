
open Core

module Subst = struct
  type t = (Identity.t * Identity.t) list
  [@@deriving compare, eq, sexp, ord, hash]

  let empty = []
  let add self (k, v) : t =
    if List.Assoc.mem self ~equal:(=) k then self
    else if k = v then self
    else (k, v) :: self

  let subst self orig_id new_id : t =
    List.map self ~f:(fun (k, v) -> (k, if v = orig_id then new_id else v)) |>
    List.filter ~f:(fun (k, v) -> k <> v)

  module T = struct
    let (@<<) mp (k, v) = (k, v) :: mp
  end

  let rec to_string = function
    | [] -> ""
    | (k, v) :: mp -> Identity.Short.show k ^ "/" ^ Identity.Short.show v ^ "," ^ to_string mp
  let to_string mp = "[" ^ to_string mp ^ "]"

  let find = List.Assoc.find
  let find_exn = List.Assoc.find_exn
end

module ObjSubst = struct
  type t = Objt.t Identity.Map.t
  [@@deriving compare, sexp]

  module Map = Identity.Map
end

module Typedef = struct
  type t =
    | UApp of UnknownPredicate.t * t list
    | Value of Objt.t
    | Op1 of Op.t * t
    | Op2 of t * Op.t * t
  [@@deriving compare, sexp, hash]

  let equal x y = compare x y = 0

  let is_uapp = function
    | UApp _ -> true | _ -> false

  let uapp x y = UApp (x, y)
  let value x = Value (x)
  let op1 x y = Op1 (x, y)
  let op2 x y z = Op2 (x, y, z)
end
include Typedef
include Showsexp.Make(Typedef)
include Hashable.Make(Typedef)
module HashRow = HashRow.Make(Typedef)
module Set = Set.Make(Typedef)

module Operation = struct
  let true_ = Value Objt.true_
  let false_ = Value Objt.false_
  let and_ x y = op2 x Op.And_ y
  let and_s x y =
    if x = Value (Objt.true_) then
      y
    else if y = Value (Objt.true_) then
      x
    else
      and_ x y
  let or_ x y = op2 x Op.Or_ y
  let var x = Value (Objt.mk_var x)

  let specialvar x = Objt.specialvar x
end
include Operation

module DSL = struct
  let true_ = true_
  let false_ = false_

  let not_ x = Op1 (Op.Not_, x)
  let (!=) x y = Op2 (x, Op.Neq, y)
  let (==>) x y = Op2 (x, Op.Impl, y)
  let (<==>) x y = Op2 (x, Op.Iff, y)
  let (==) x y = Op2 (x, Op.Eq, y)

  let (%+) x y = Op2 (x, Op.Plus, y)
  let (%-) x y = Op2 (x, Op.Minus, y)
  let (%*) x y = Op2 (x, Op.Times, y)
  let (<=) x y = Op2 (x, Op.Leq, y)
  let (>=) x y = Op2 (x, Op.Geq, y)
  let (<) x y = Op2 (x, Op.Lt, y)

  let (&&) x y = and_ x y
  let (||) x y = or_ x y

  let int_ x = Value (Objt.mk_int x)
  let bool_ x = Value (Objt.mk_bool x)
  let var x = Value (Objt.mk_var x)
  let value x = Value x

  let neg x = Op1 (Op.Minus, x)

  let forall v self = Op1 (Op.Forall v, self)
  let exists v self = Op1 (Op.Exists v, self)
end
module T = DSL

module UApp_plus = struct
  let rec map ~f = function
    | UApp (ut, x) -> f (ut, x)
    | Op1 (op, c1) -> Op1 (op, map ~f c1)
    | Op2 (c1, op, c2) -> Op2 (map ~f c1, op, map ~f c2)
    | x -> x

  let rec map_multi ~f = function
    | UApp (ut, x) -> f (ut, x)
    | Op1 (op, c1) -> List.map (map_multi ~f c1) ~f:(fun x -> Op1 (op, x))
    | Op2 (c1, op, c2) ->
      List.map (map_multi ~f c1) ~f:(fun x ->
        List.map (map_multi ~f c2) ~f:(fun y ->
          Op2 (x, op, y)
        )
      ) |> List.concat
    | x -> [x]

  let rec fold ~f ~init = function
    | UApp (ut, x) -> f init (ut, x)
    | Op1 (op, c1) -> Op1 (op, fold ~f ~init c1)
    | Op2 (c1, op, c2) -> Op2 (fold ~f ~init c1, op, fold ~f ~init c2)
    | x -> x
end

module Visualized = struct
  module T = struct
    type t =
      | V_UApp of UnknownPredicate.t * t list
      | V_Value of Objt.t
      | V_Op1 of Op.t * t
      | V_Qua of Op.t * Identity.t list * t
      | V_Op2 of Op.t * t list
    [@@deriving compare, eq, sexp, hash]
  end
include T
 module Set = Core.Set.Make(T)

  module Operator = struct
    let is_right_connective (op : Op.t) : bool =
      let open Op in
      match op with
      | Plus -> true
      | Times -> true
      | And_ -> true
      | Or_ -> true
      | Impl -> true
      | _ -> false

    let is_left_connective (op : Op.t) : bool =
      let open Op in
      match op with
      | Plus -> true
      | Minus -> true
      | Times -> true
      | Div -> true
      | And_ -> true
      | Or_ -> true
      | _ -> false
  end

  let of_cond (cond : Typedef.t) : t =
    let rec build (cond : Typedef.t) : t =
      match cond with
      | UApp (ut, x) -> V_UApp (ut, List.map ~f:build x)
      | Value v -> V_Value v
      | Op1 (op, c1) when Op.is_quantifier op ->
        begin
          match op with
          | Op.Forall v -> V_Qua (op, [v], build c1)
          | Op.Exists v -> V_Qua (op, [v], build c1)
          | _ -> failwith "unexpected"
        end
      | Op1 (op, c1) -> V_Op1 (op, build c1)
      | Op2 (c1, op, c2) -> V_Op2 (op, [build c1; build c2]) in
    let rec flatten (self : t) : t =
      match self with
      | V_Op1 (op, c) -> V_Op1 (op, flatten c)
      | V_Op2 (op, cs) when Operator.is_right_connective op && Operator.is_left_connective op ->
        let flatten_cs =
          List.fold_right cs ~init:[] ~f:(fun term acc ->
            match flatten term with
            | V_Op2 (op', cs') when Op.equal op' op -> cs' @ acc
            | x -> x :: acc
          ) in V_Op2 (op, flatten_cs)
      | V_Op2 (op, cs) when Operator.is_right_connective op ->
        begin
          let cs = List.map cs ~f:flatten in
          match List.split_n cs (List.length cs - 1) with
          | (hds, [tl]) ->
            begin match tl with
              | V_Op2 (op', cs') when Op.equal op' op -> V_Op2 (op, hds @ cs')
              | x -> V_Op2 (op, cs)
            end
          | _ -> V_Op2 (op, cs)
        end
      | V_Op2 (op, cs) when Operator.is_left_connective op ->
        begin
          match List.map cs ~f:flatten with
          | hd :: tls ->
            begin match hd with
              | V_Op2 (op', cs') when Op.equal op' op -> V_Op2 (op, cs' @ tls)
              | _ -> V_Op2 (op, hd :: tls)
            end
          | cs -> V_Op2 (op, cs)
        end
      | V_Op2 (op, cs) -> V_Op2 (op, List.map cs ~f:flatten)
      | V_Qua (op, vs, c) ->
        begin
          match flatten c with
          | V_Qua (op', vs', c') when Op.equal_quantifier op op' -> V_Qua (op, vs @ vs', c')
          | x -> V_Qua (op, vs, x)
        end
      | V_UApp (ut, x) -> V_UApp (ut, List.map ~f:flatten x)
      | x -> x
    in flatten (build cond)

  let rec to_cond (self : t) : Typedef.t =
    match self with
    | V_Op1 (op, c) -> Op1 (op, to_cond c)
    | V_Op2 (op, cs) when Operator.is_right_connective op ->
      begin
        match List.split_n cs (List.length cs - 1) with
        | (cs, [tl]) -> List.fold_right cs ~init:(to_cond tl) ~f:(fun x acc -> Op2 (to_cond x, op, acc))
        | (_, _) -> Value Objt.true_
      end
    | V_Op2 (op, cs) ->
      begin
        match (List.hd cs, List.tl cs) with
        | (Some hd, Some cs) -> List.fold cs ~init:(to_cond hd) ~f:(fun acc x -> Op2 (acc, op, to_cond x))
        | (_, _) -> Value Objt.true_
      end
    | V_Qua (op, vs, c) ->
      let mk_op =
        begin
          match op with
          | Op.Forall _ -> (fun v -> Op.Forall v)
          | Op.Exists _ -> (fun v -> Op.Exists v)
          | _ -> failwith "unexpected"
        end
      in
      List.fold_right vs ~init:(to_cond c) ~f:(fun v acc ->
        Op1 (mk_op v, acc)
      )
    | V_UApp (ut, x) -> UApp (ut, List.map ~f:to_cond x)
    | V_Value v -> Value v

  include Formatable.Make(struct
    module I = Formatable

    type nonrec t = t
    let rec join els sep =
      match els with
      | [] -> []
      | [el] -> [el]
      | el :: els' -> el :: sep :: join els' sep

    let rec to_format (term : t) : I.t =
      match term with
      | V_Value obj ->
          Objt.to_format obj;
      | V_UApp (un, cs) ->
          let content = List.map cs ~f:to_format in
          let content = join content (I.text (", ")) in
          I.inline [
            I.text (UnknownPredicate.show_id un);
            I.text "(";
            I.inline content;
            I.text ")";
          ]
      | V_Op1 (op, c) ->
        I.inline [
          I.text "(";
          I.text (Op.string_of op ^ " ");
          to_format c;
          I.text ")";
        ]
      | V_Op2 (op, cs) ->
        let content = List.map cs ~f:to_format in
        let content = join content (I.text (" " ^ Op.string_of op ^ " ")) in
        I.inline [I.text "("; I.inline content; I.text ")";]
      | V_Qua (op, vs, c) ->
        I.inline [
          I.text (Op.quantifier_label op);
          I.text (List.to_string vs ~f:(fun v -> Identity.Short.show v));
          I.text ".";
          to_format c;
        ]
  end)
end

include Formatable.Make(struct
  module I = Formatable

  type nonrec t = t
  let rec to_format term =
    Visualized.to_format (Visualized.of_cond term)
    (* match term with
    | Op2 (t1, op, t2) ->
        I.inline [
          I.text "(";
          to_format t1;
          I.text " ";
          I.text (Op.string_of op);
          I.text " ";
          to_format t2;
          I.text ")";
        ]
    | Op1 (op, t) ->
        I.inline [
          I.text (Op.string_of op);
          I.text " ";
          to_format t;
        ]
    | Value obj ->
        Objt.to_format obj;
    | UApp un ->
        I.noline (UApp.to_string un);
    *)
end)









module UnknownApp = struct
  type t = UnknownPredicate.t * Typedef.t list
  [@@deriving compare, eq,sexp, hash]

  let from_predicate (predicate : UnknownPredicate.t) : t =
    (predicate, List.map (UnknownPredicate.vids_of predicate) ~f:(fun v -> Value (Objt.mk_var v)))

  let assign_cond (self : Typedef.t) (var : Identity.t) (cond : Typedef.t) : Typedef.t =
    let vobj = Objt.mk_var var in
    let rec f self =
      match self with
      | Value (v) -> if Objt.equal v vobj then cond else Value v
      | UApp (u, y) -> UApp (u, List.map y ~f)
      | Op1 (x, y) -> Op1 (x, f y)
      | Op2 (x, y, z) -> Op2 (f x, y, f z)
    in f self

  let substs_of ((un, cs) : t) : (Identity.t * Typedef.t) list =
    let vids = UnknownPredicate.vids_of un in
    Option.value_exn (List.zip vids cs)

  let predicate_of ((un, cs) : t) : UnknownPredicate.t = un
  let predicate_vids_of (self : t) : Identity.t list = UnknownPredicate.vids_of (predicate_of self)

  let conds_map ~f ((un, cs) : t) : t = (un, List.map cs ~f)

  let subst (self : t) (cond : Typedef.t) : Typedef.t =
    List.fold (substs_of self) ~init:cond ~f:(fun cond (orig_var, new_cond) ->
      assign_cond cond orig_var new_cond
    )

  let cond_to_obj (cond : Typedef.t) : Objt.t option =
    match cond with
    | Value v -> Some v
    | _ -> None

  let to_cond ((un, cs) : t) : Typedef.t = UApp (un, cs)

  let to_string ((un, cs) : t) : string =
    let cstring = List.map cs ~f:to_string |> Util.join ", " in
    UnknownPredicate.id_of un ^ "(" ^ cstring ^ ")"
end

module UnknownSubst = struct
  module Map = UnknownPredicate.Map
  type t = Typedef.t Map.t

  let assign (self : t) (cond : Typedef.t) : Typedef.t =
    UApp_plus.map cond ~f:(fun uapp ->
      match Map.find self (UnknownApp.predicate_of uapp) with
      | Some x -> UnknownApp.subst uapp x
      | None -> UnknownApp.to_cond uapp
    )

  let of_alist (un_conds : (UnknownPredicate.t * Typedef.t) list) : t option =
    match Map.of_alist un_conds with
    | `Ok v -> Some v
    | _ -> None

  let to_string (self : t) : string =
    List.fold (Map.to_alist self) ~init:"\n" ~f:(fun str (key, data) ->
      UnknownPredicate.to_string key ^ ": " ^ to_string data ^ "\n" ^ str
    )
end

module Simplifier = struct
  module Eq = struct
    let rec simplify cond =
      match cond with
      | Op1 (op, c1) -> Op1 (op, simplify c1)
      | Op2 (Value v1, Op.Eq, Value v2) when Objt.is_var v1 && Objt.is_var v2 ->
        let vid1 = Objt.vid_of_exn v1 in
        let vid2 = Objt.vid_of_exn v2 in
        if vid1 = vid2 then Value Objt.true_ else cond
      | Op2 (c1, op, c2) ->
        Op2 (simplify c1, op, simplify c2)
      | x -> x
  end

  module Bool = struct
    let rec get_bool term =
      match term with
      | Value obj -> Objt.get_bool obj
      | _ -> None

    let calculate1 op t =
      let open Op in
      match (op, get_bool t) with
      | (Not_, Some b) -> Some (Value (Objt.mk_bool (not b)))
      | (_, _) -> None

    let calculate2 op t1 t2 =
      let open Op in
      match (op, get_bool t1, get_bool t2) with
      | (And_, Some true, _) -> Some t2
      | (And_, _, Some true) -> Some t1
      | (And_, Some false, _) -> Some (Value Objt.false_)
      | (And_, _, Some false) -> Some (Value Objt.false_)
      | (Or_, Some true, _) -> Some (Value Objt.true_)
      | (Or_, _, Some true) -> Some (Value Objt.true_)
      | (Or_, Some false, _) -> Some t2
      | (Or_, _, Some false) -> Some t1
      | (Impl, Some true, _) -> Some t2
      | (Impl, _, Some true) -> Some (Value Objt.true_)
      | (Impl, Some false, _) -> Some (Value Objt.true_)
      (* | (Impl, _, Some false) -> Some (Op1 (Not_, t1)) *)
      | (_, _, _) -> None

    let rec simplify term =
      match term with
      | Op1 (op, t1) ->
          let t1 = simplify t1 in
          Option.value (calculate1 op t1) ~default:(Op1 (op, t1))
      | Op2 (t1, op, t2) ->
          let t1 = simplify t1 in
          let t2 = simplify t2 in
          Option.value (calculate2 op t1 t2) ~default:(Op2 (t1, op, t2))
      | x -> x
  end

  module Num = struct
    let rec get_int term =
      match term with
      | Value obj -> Objt.get_int obj
      | _ -> None

    let calculate1 op t =
      let open Op in
      match (op, get_int t) with
      | (Minus, Some b) -> Some (Value (Objt.mk_int (- b)))
      | (_, _) -> None

    let calculate2 op t1 t2 =
      let open Op in
      match (op, get_int t1, get_int t2) with
      | (Plus, Some 0, _) -> Some t2
      | (Plus, _, Some 0) -> Some t1
      | (Times, Some 0, _) -> Some (Value (Objt.mk_int 0))
      | (Times, _, Some 0) -> Some (Value (Objt.mk_int 0))
      | (Times, Some 1, _) -> Some (t2)
      | (Times, _, Some 1) -> Some (t1)
      | (Times, Some (- 1), _) -> Some (Op1 (Op.Minus, t2))
      | (Times, _, Some (- 1)) -> Some (Op1 (Op.Minus, t1))
      | (Leq, Some a, Some b) when a <= b -> Some (Value Objt.true_)
      | (Leq, Some a, Some b) when a > b -> Some (Value Objt.false_)
      | (Lt, Some a, Some b) when a < b -> Some (Value Objt.true_)
      | (Lt, Some a, Some b) when a >= b -> Some (Value Objt.false_)
      | (Geq, Some a, Some b) when a >= b -> Some (Value Objt.true_)
      | (Geq, Some a, Some b) when a < b -> Some (Value Objt.false_)
      | (Gt, Some a, Some b) when a > b -> Some (Value Objt.true_)
      | (Gt, Some a, Some b) when a <= b -> Some (Value Objt.false_)
      | (_, _, _) -> None

    let rec simplify term =
      match term with
      | Op1 (op, t1) ->
          let t1 = simplify t1 in
          Option.value (calculate1 op t1) ~default:(Op1 (op, t1))
      | Op2 (t1, op, t2) ->
          let t1 = simplify t1 in
          let t2 = simplify t2 in
          Option.value (calculate2 op t1 t2) ~default:(Op2 (t1, op, t2))
      | x -> x
  end

  module Ice = struct
    let rec sort term =
      let open Op in
      match term with
      | Op2 (
          Op2 (Value (Objt.IntObj c1), Op.Times, t1), Op.Plus,
          Op2 (Value (Objt.IntObj c2), Op.Times, t2)
          ) when c1 < c2 ->
        sort (Op2 (
          Op2 (Value (Objt.IntObj c2), Op.Times, t2), Op.Plus,
          Op2 (Value (Objt.IntObj c1), Op.Times, t1)
          ))
      | Op2 (
          Op2 (
            Op2 (Value (Objt.IntObj c1), Op.Times, t1), Op.Plus,
            Op2 (Value (Objt.IntObj c2), Op.Times, t2)
          ),
          Op.Leq,
          Value (Objt.IntObj c3)
        ) when c1 <= 0 && c2 <= 0 ->
        Op2 (
          Op2 (
            Op2 (Value (Objt.IntObj (- c1)), Op.Times, t1), Op.Plus,
            Op2 (Value (Objt.IntObj (- c2)), Op.Times, t2)
          ),
          Op.Geq,
          Value (Objt.IntObj (- c3))
        )
      | Op2 (t1, Op.And_, t2) when (Typedef.equal t1 t2) -> t1
      | Op2 (t1, Op.Or_, t2) when (Typedef.equal t1 t2) -> t1
      | _ -> term

    let rec simplify term =
      let open Op in
      match term with
      | Op1 (op, t1) ->
          let t1 = simplify t1 in
          Op1 (op, t1)
      | Op2 (t1, op, t2) ->
          let t1 = simplify t1 in
          let t2 = simplify t2 in
          sort (Op2 (t1, op, t2))
      | x -> x
  end

  module RemoveDuplicated = struct
    open Visualized

    let rec remove_duplicated (self : t) : t =
      match self with
      | V_Op1 (op, c) -> V_Op1 (op, remove_duplicated c)
      | V_Op2 (op, cs) ->
        let cs = List.map cs ~f:remove_duplicated in
        let reduce cs = cs |> Set.of_list |> Set.to_list in
        begin
          match op with
          | Op.And_ -> V_Op2 (op, reduce cs)
          | Op.Or_ -> V_Op2 (op, reduce cs)
          | Op.Iff -> V_Op2 (op, reduce cs)
          | _ -> V_Op2 (op, cs)
        end
      | V_Qua (op, vs, c) -> V_Qua (op, vs, remove_duplicated c)
      | x -> x

    let simplify (c : Typedef.t) : Typedef.t = of_cond c |> remove_duplicated |> to_cond
  end

  let simplify term = term |> Eq.simplify |> Ice.simplify |> Num.simplify |> RemoveDuplicated.simplify |> Bool.simplify

  include Formatable.Make(struct
    type nonrec t = t
    let to_format term = to_format (simplify term)
  end)
end

module Destruction = struct
  let obj_of x =
    match x with
    | Value obj -> Some obj
    | _ -> None

  let is_var x =
    match x with
    | Value obj -> Objt.is_var obj
    | _ -> false

  let vid_exn x =
    match x with
    | Value obj -> Objt.vid_of_exn obj
    | _ -> failwith "not variable"

  let var_of x =
    match x with
    | Value obj when Objt.is_var obj -> Some (Objt.vid_of_exn obj)
    | _ -> None

  let rec uapps_of = function
    | UApp (un, cs) -> [(un, cs)]
    | Op1 (op, t) -> uapps_of t
    | Op2 (t1, op, t2) -> uapps_of t1 @ uapps_of t2
    | _ -> []

  let uapp_of (self : t) : UnknownApp.t option =
    match uapps_of self with
    | [] -> None
    | x :: _ -> Some x
end
include Destruction

let rec subst cond orig_var new_var =
  match cond with
  | UApp (un, cs) -> UApp (un, List.map cs ~f:(fun x -> subst x orig_var new_var))
  | Value obj -> Value (Objt.subst obj (orig_var, Objt.mk_var new_var))
  | Op1 (op, t) -> Op1 (op, subst t orig_var new_var)
  | Op2 (t1, op, t2) -> Op2 (subst t1 orig_var new_var, op, subst t2 orig_var new_var)

let rec subst_cond cond (target, cond') =
  if [%compare.equal: t] cond target
  then cond'
  else
    match cond with
    | UApp (un, cs) -> UApp (un, List.map cs ~f:(fun x -> subst_cond x (target, cond')))
    | Op1 (op, t) -> Op1 (op, subst_cond t (target, cond'))
    | Op2 (t1, op, t2) -> Op2 (subst_cond t1 (target, cond'), op, subst_cond t2 (target, cond'))
    | _ -> cond

let rec has cond target =
  if [%compare.equal: t] cond target
  then true
  else
    match cond with
    | Op1 (op, t) -> has t target
    | Op2 (t1, op, t2) -> has t1 target || has t2 target
    | _ -> false

module Variants = struct
  let map self ~value ~op1 ~op2 ~uapp =
    match self with
    | Value (x) -> value self x
    | UApp (x, y) -> uapp self x y
    | Op1 (x, y) -> op1 self x y
    | Op2 (x, y, z) -> op2 self x y z
end

let rec assign_obj (self : t) (var : Identity.t) (obj : Objt.t) : t =
  match self with
  | Value (v) -> value (Objt.subst v (var, obj))
  | UApp (u, y) -> UApp (u, List.map y ~f:(fun x -> assign_obj x var obj))
  | Op1 (x, y) -> Op1 (x, assign_obj y var obj)
  | Op2 (x, y, z) -> Op2 (assign_obj x var obj, y, assign_obj z var obj)

let rec get_vids cond =
  let value _ obj = Objt.get_vids obj in
  let op1 _ _ t = get_vids t in
  let op2 _ t1 _ t2 = get_vids t1 @ get_vids t2 in
  let uapp _ _ y = List.map y get_vids |> List.concat in
  Variants.map cond ~value ~op1 ~op2 ~uapp

let vids_of cond = get_vids cond

let rec uapps cond =
  let value _ _ = [] in
  let op1 _ _ t = uapps t in
  let op2 _ t1 _ t2 = uapps t1 @ uapps t2 in
  let uapp _ un y = [(un, y)] in
  Variants.map cond ~value ~op1 ~op2 ~uapp

let unneg cond =
  match cond with
  | Op1 (Op.Not_, cond') -> cond'
  | _ -> cond

module Horn = struct
  type nonrec t = t list

  let map = List.map
  let fold = List.fold

  let rec elim_not cond =
    match cond with
    | Op2 (t1, op, t2) ->
        Op2 (elim_not t1, op, elim_not t2)
    | Op1 (Op.Not_, t1) ->
        Op2 (elim_not t1, Op.Impl, value Objt.false_)
    | Op1 (op, t1) ->
        Op1 (op, elim_not t1)
    | _ -> cond

  let elim_nested_impl cond =
    let rec f c1 c2 =
      match c2 with
      | Op2 (t1, Op.Impl, t2) ->
          f (Op2 (c1, Op.And_, t1)) t2
      | Op2 (t1, Op.And_, t2) ->
          begin match t2 with
          | Op2 (_, Op.Impl, _) -> Op2 (f c1 t1, Op.And_, f c1 t2)
          | _ -> Op2 (c1, Op.Impl, c2)
          end
      | _ -> Op2 (c1, Op.Impl, c2)
    in
    let rec g cond =
      match cond with
      | Op2 (t1, Op.Impl, t2) -> f t1 t2
      | Op2 (t1, op, t2) -> Op2 (g t1, op, g t2)
      | Op1 (op, t1) -> Op1 (op, g t1)
      | _ -> cond
    in g cond

  let hornize cond = elim_nested_impl (Simplifier.simplify cond)

  let main cond : t =
    let rec f cond =
      match cond with
      | Op2 (t1, op, t2) ->
          begin
            let open Op in
            match op with
            | And_ -> f t1 @ f t2
            | _ -> [cond]
          end
      | _ -> [cond]
    in f (hornize cond)

  let get_horn_clauses cond : Typedef.t list =
    let rec f cond =
      match cond with
      | Op2 (t1, Op.Impl, t2) ->
        f t1 @ [t2]
      | Op2 (t1, Op.And_, t2) ->
        f t1 @ f t2
      | Op1 (op, t1) when Op.is_quantifier op ->
        f t1
      | _ -> [cond]
    in
    f (hornize cond)

  include Formatable.Make(struct
    type nonrec t = t
    module I = Formatable
    let to_format clauses =
      I.block (List.map clauses ~f:(fun x -> x |> to_format |> I.to_block))
  end)

  module Impl_plus = struct
    type t = Typedef.t

    let map cond ~f =
      let rec g cond =
        match cond with
        | Op2 (t1, Op.Impl, t2) ->
            g t1 @ g t2
        | Op2 (t1, op, t2) -> [f cond]
        | _ -> [f cond]
      in g cond

    let to_alist cond = map cond ~f:ident
    let to_tuple cond =
      match cond with | Op2 (t1, Op.Impl, t2) -> (t1, t2)
      | _ -> (value Objt.true_, cond)

  end
end

include Quantifiable.Make(struct
  type nonrec t = t

  let mk_or t1 t2 = Op2 (t1, Op.Or_, t2)
  let mk_and t1 t2 = Op2 (t1, Op.And_, t2)
  let mk_false = Value Objt.false_
  let mk_true = Value Objt.true_
end)

module SimpleTypeInfer = struct
  let main self : SimpleType.Env.tt =
    let tyenv = List.fold (get_vids self) ~init:SimpleType.Env.empty ~f:(fun acc vid ->
      SimpleType.Env.T.(acc %<< (vid, SimpleType.gen_var ()))
    ) in
    let rec (infer : SimpleType.Relation.t -> t -> SimpleType.Relation.t * SimpleType.t) =
      fun tyrel cond ->
      match cond with
      | Value v when is_var cond ->
          let vid = vid_exn cond in
          let rtn_ty = SimpleType.gen_var () in
          let tyrel = SimpleType.Relation.T.(tyrel %<< (SimpleType.Env.find_exn tyenv vid == rtn_ty)) in
          (tyrel, rtn_ty)
      | Value v ->
          (tyrel, if Objt.is_int v then SimpleType.int_ else SimpleType.bool_)
      | Op2 (t1, op, t2) ->
          let (tyrel, ty1) = infer tyrel t1 in
          let (tyrel, ty2) = infer tyrel t2 in
          let arg_ty =
            if Op.is_arg_polymorphic op then SimpleType.gen_var ()
            else if Op.is_arg_bool op then SimpleType.bool_
            else SimpleType.int_
          in
          let rtn_ty = if Op.is_value_bool op then SimpleType.bool_ else SimpleType.int_ in
          (SimpleType.Relation.T.(tyrel %<< (ty1 == arg_ty) %<< (ty2 == arg_ty)), rtn_ty)
      | Op1 (op, t1) ->
          let (tyrel, ty1) = infer tyrel t1 in
          let arg_ty = if Op.is_arg_bool op then SimpleType.bool_ else SimpleType.int_ in
          let rtn_ty = if Op.is_value_bool op then SimpleType.bool_ else SimpleType.int_ in
          (SimpleType.Relation.T.(tyrel %<< (ty1 == arg_ty)), rtn_ty)
      | UApp _ ->
          (tyrel, SimpleType.bool_)
    in
    let (tyrel, ty) = infer SimpleType.Relation.empty self in
    let tyrel = SimpleType.Relation.T.(tyrel %<< (ty == SimpleType.bool_)) in
    SimpleType.Relation.unify tyenv tyrel

  let get_type self tyenv : SimpleType.t =
    match self with
    | Value v when is_var self ->
        let vid = vid_exn self in
        SimpleType.Env.find_exn tyenv vid
    | Value v ->
        if Objt.is_int v then SimpleType.int_ else SimpleType.bool_
    | Op2 (t1, op, t2) ->
        if Op.is_value_bool op then SimpleType.bool_ else SimpleType.int_
    | Op1 (op, t1) ->
        if Op.is_value_bool op then SimpleType.bool_ else SimpleType.int_
    | UApp _ ->
        SimpleType.bool_
end

module ElimFreeVar = struct
  module Equality = struct
    type t = (Identity.t * Identity.t) list

    let is_empty = List.is_empty

    let normalize (self : t) bind_vars : t =
      let sbst (x, y) a = if Identity.equal x a then y else a in
      let rec loop ?(acc = []) (self : t) : t =
        let self = List.filter self ~f:(fun (x, y) -> not (Identity.equal x y || Identity.Set.mem bind_vars x)) in
        match self with
        | [] -> List.rev acc
        | hd :: zs ->
          loop ~acc:(hd :: acc) (List.map zs ~f:(fun (a, b) -> (sbst hd a, sbst hd b)))
      in
      loop self

    let apply (self : t) (cond : Typedef.t) : Typedef.t =
      List.fold self ~init:cond ~f:(fun cond (x, y) -> subst cond x y) |> Simplifier.simplify
  end

  let rec run cond ~bind_vars =
    let rec gather_equalities cond : Equality.t =
      match cond with
      | Op1 (op, c1) -> [] (* gather_equalities c1 *)
      | Op2 (Value v1, Op.Eq, Value v2) when Objt.is_var v1 && Objt.is_var v2 ->
        let vid1 = Objt.vid_of_exn v1 in
        let vid2 = Objt.vid_of_exn v2 in
        [(vid1, vid2); (vid2, vid1)]
      | Op2 (c1, Op.And_, c2) ->
        gather_equalities c1 @ gather_equalities c2
      | _ -> []
    in
    let equalities = gather_equalities cond in
    let equalities = Equality.normalize equalities bind_vars in
    if Equality.is_empty equalities then
      cond
    else
      run (Equality.apply equalities cond) ~bind_vars
end

module Boolnize = struct
  type nonrec t = t

  let falsy_value = Value (Objt.mk_int 0)
  let btoi v = Op1 (Op.BtoI, v)
  let itob v = Op1 (Op.ItoB, v)

  let main self =
    let tyenv = List.fold (get_vids self) ~init:SimpleType.Env.empty ~f:(fun tyenv vid ->
      SimpleType.Env.T.(tyenv %<< (vid, SimpleType.Int_))
    ) in
    let rec boolnize cond =
      match cond with
      | Value v ->
          if Objt.is_bool v
          then cond
          else itob cond
      | Op1 (op, c1) when Op.is_quantifier op -> Op1 (op, boolnize c1)
      | Op1 (op, c1) when Op.is_value_bool op ->
          if Op.is_arg_bool op
          then Op1 (op, boolnize c1)
          else Op1 (op, intnize c1)
      | Op1 (op, c1) ->
          if Op.is_arg_bool op
          then itob (Op1 (op, boolnize c1))
          else itob (Op1 (op, intnize c1))
      | Op2 (c1, Op.Eq, c2) ->
          begin match (SimpleTypeInfer.get_type c1 tyenv, SimpleTypeInfer.get_type c2 tyenv) with
          | (SimpleType.Bool_, _) -> Op2 (boolnize c1, Op.Iff, boolnize c2)
          | (_, SimpleType.Bool_) -> Op2 (boolnize c1, Op.Iff, boolnize c2)
          | (_, _) -> Op2 (intnize c1, Op.Eq, intnize c2)
          end
      | Op2 (c1, op, c2) when Op.is_value_bool op ->
          if Op.is_arg_bool op
          then Op2 (boolnize c1, op, boolnize c2)
          else Op2 (intnize c1, op, intnize c2)
      | Op2 (c1, op, c2) ->
          if Op.is_arg_bool op
          then itob (Op2 (boolnize c1, op, boolnize c2))
          else itob (Op2 (intnize c1, op, intnize c2))
      | _ -> cond
    and intnize cond =
      match cond with
      | Value v ->
          if Objt.is_bool v
          then cond
          else cond
      | Op1 (op, c1) when Op.is_quantifier op -> Op1 (op, intnize c1)
      | Op1 (op, c1) when Op.is_value_bool op -> btoi (boolnize cond)
      | Op1 (op, c1) ->
          if Op.is_arg_bool op
          then Op1 (op, boolnize c1)
          else Op1 (op, intnize c1)
      | Op2 (c1, Op.Eq, c2) -> btoi (boolnize cond)
      | Op2 (c1, op, c2) when Op.is_value_bool op -> btoi (boolnize cond)
      | Op2 (c1, op, c2) ->
          if Op.is_arg_bool op
          then Op2 (boolnize c1, op, boolnize c2)
          else Op2 (intnize c1, op, intnize c2)
      | _ -> cond
    in boolnize self
end

module Eval = struct
  type nonrec t = Typedef.t

  let rec eval ?f (term : t) : t =
    let hook = Option.value f ~default:(fun x -> x) in
    let eval = eval ~f:hook in
    match hook term with
    | Op1 (op, t1) ->
        let t1 = eval t1 in
        Option.value (
          match op with
          | o when Op.is_quantifier o -> Some t1
          | Op.Not_ -> Option.map (to_bool t1) ~f:(fun x -> T.bool_ (not x))
          | Op.Minus -> Option.map (to_int t1) ~f:(fun x -> T.int_ (- x))
          (* TODO: Write conversion *)
          | Op.BtoI -> Option.map (to_bool t1) ~f:(fun x -> T.bool_ x)
          | Op.ItoB -> Option.map (to_int t1)  ~f:(fun x -> T.int_ x)
          | _ -> None
        ) ~default:(Op1 (op, t1))
    | Op2 (t1, op, t2) ->
        let tp = function (Some x, Some y) -> Some (x, y) | _ -> None in
        let t1 = eval t1 in
        let t2 = eval t2 in
        let term = Op2 (t1, op, t2) in
        if Op.is_arg_bool op
        then
          Option.map ((t1 |> to_bool, t2 |> to_bool) |> tp) (fun (t1, t2) ->
            match op with
            | Op.And_ -> T.bool_ (t1 && t2)
            | Op.Or_ ->  T.bool_ (t1 || t2)
            | Op.Iff ->  T.bool_ (t1 ==> t2 && t2 ==> t1)
            | Op.Impl -> T.bool_ (t1 ==> t2)
            | _ -> term
          ) |> Option.value ~default:term
        else
          Option.map ((t1 |> to_int, t2 |> to_int) |> tp) (fun (t1, t2) ->
            match op with
            | Op.Eq ->   T.bool_ (t1 = t2)
            | Op.Neq ->  T.bool_ (t1 <> t2)
            | Op.Leq ->  T.bool_ (t1 <= t2)
            | Op.Geq ->  T.bool_ (t1 >= t2)
            | Op.Lt ->   T.bool_ (t1 <  t2)
            | Op.Gt ->   T.bool_ (t1 >  t2)
            | Op.Plus ->  T.int_ (t1 + t2)
            | Op.Minus -> T.int_ (t1 - t2)
            | Op.Times -> T.int_ (t1 * t2)
            | Op.Div ->
              if true || Int.rem t1 t2 = 0 then
                T.int_ (t1 / t2)
              else
                DSL.(Op2 (int_ t1, op, int_ t2))
            | _ -> term
          ) |> Option.value ~default:term
    | x -> x

  and to_bool (term : t) : bool option =
    match term with
    | Value v ->
        Objt.get_bool v
    | _ -> None

  and to_int (term : t) : int option =
    match term with
    | Value v ->
        Objt.get_int v
    | _ -> None

  let eval_to_bool (term : t) : bool option =
    let ob = term |> eval |> to_bool in (* Option.map ob ~f:(fun b -> log term; Logger.debug (Bool.to_string b)); *) ob
  let eval_to_int (term : t) : int option = term |> eval |> to_int

  let eval_with_hook (term : t) (hook : t -> t) : t =
    eval term ~f:hook
end

module AlphaConv = struct
  module L = Label.Make(struct let label= "cond-alpha-rename" end)

  let run (self : t) ~vids_not_to_rename =
    let vids = vids_of self |> Identity.Set.of_list in
    let vids_to_rename = Identity.Set.diff vids vids_not_to_rename in
    Identity.Set.fold vids_to_rename ~init:self ~f:(fun self vid ->
      let new_vid = L.gen () in
      subst self vid new_vid
    )
end


let rec get_apps t =
  match t with
  | UApp(p, _) -> [p]
  | Value v -> []
  | Op1(_, t) -> get_apps t
  | Op2(t1, _, t2) -> get_apps t1 @ get_apps t2

let get_pids t = List.map ~f:UnknownPredicate.id_of @@ get_apps t


let rec get_fv t =
  match t with
  | UApp(p, ts) -> List.concat @@ List.map ~f:get_fv ts
  | Value v -> Objt.get_fv v
  | Op1(_, t) -> get_fv t
  | Op2(t1, _, t2) -> get_fv t1 @ get_fv t2


let rec flatten t =
  match t with
  | Op2(t1, Op.Impl, t2) ->
      begin
        match t2 with
        | Op2(t21, Op.And_, t22) ->
            flatten DSL.(t1 ==> t21) @
            flatten DSL.(t1 ==> t22)
        | Op2(t21, Op.Impl, t22) ->
            flatten DSL.((t1 && t21) ==> t22)
        | _ -> [t]
      end
  | _ -> [t]


let rec decomp_ands t =
  match t with
  | Op2(t1, Op.And_, t2) -> decomp_ands t1 @ decomp_ands t2
  | _ -> [t]

let ands ts =
  match ts with
  | [] -> DSL.true_
  | t::ts' -> List.fold_left ~f:and_s ~init:t ts'

let rec decomp_hc ?(full=false) t =
  match t with
  | Op2(t1, Op.Impl, t2) ->
      let ts =
        if full then
          decomp_ands t1
        else
          [t1]
      in
      ts, t2
  | _ -> [], t
let compose_hc ts t =
  Op2(ands ts, Op.Impl, t)

let rec alpha_rename (t : t) =
  let ts,t2 = decomp_hc t in
  match t2 with
  | Value _ -> t
  | UApp(pred, ts2) ->
      let fv = get_fv t2 in
      let ts2',map =
        let aux t =
          match t with
          | Value (Objt.VarObj x) when 1 = List.length (List.filter ~f:((=) x) fv) ->
              t, None
          | t ->
              let y = AlphaConv.L.gen() in
              DSL.var y, Some (y, t)
        in
        List.unzip @@ List.map ~f:aux ts2
      in
      let map' = List.filter_opt map in
      let ts' = List.fold_left ~f:(fun ts (x,t) -> DSL.(var x == t)::ts) ~init:ts map' in
      compose_hc ts' (UApp(pred,ts2'))
  | _ -> assert false


let rec is_simple t =
  match t with
  | Value _ -> true
  | Op2(t1, _, t2) -> is_simple t1 && is_simple t2
  | Op1(_, t1) -> is_simple t1
  | _ -> false

let subst_simple_exp t =
  let body,head = decomp_hc ~full:true t in
  let rec sbst rest body head =
    match rest with
    | [] -> compose_hc body head
    | t::rest' ->
        let map =
          match t with
          | Op2(Value (Objt.VarObj x), Op.Eq, t') when is_simple t' -> Some (
            x, t'
          )
          | Op2(t', Op.Eq, Value (Objt.VarObj x)) when is_simple t' -> Some (
            x, t'
          )
          | _ -> None
        in
        match map with
        | None -> sbst rest' (t::body) head
        | Some (x,t) ->
            let f t' = UnknownApp.assign_cond t' x t in
            sbst (List.map ~f rest') (List.map ~f body) (f head)
  in
  sbst body [] head

let simplify_alpha t = t |> subst_simple_exp |> alpha_rename


module ToSmt2 = struct

  let rec term_fmt fmt (term: t) = match term with
  | Value (Objt.VarObj vid) -> Format.fprintf fmt "|%s|" vid
  | Value (Objt.SpecialVar vid) -> failwith "special var are not supported"
  | Value (Objt.IntObj i) -> (
    if i >= 0 then Format.fprintf fmt "%d" i
    else Format.fprintf fmt "(- %d)" (-i)
  )
  | Value (Objt.BoolObj b) -> Format.fprintf fmt "%b" b
  | Value (Objt.Array _) -> failwith "arrays are not supported"
  | UApp (un, terms) -> (
    Format.fprintf fmt "(|%s|" (Identity.Short.show un.UnknownPredicate.id) ;
    List.iter terms (Format.fprintf fmt " %a" term_fmt) ;
    Format.fprintf fmt ")"
  )
  | Op1 (op, term) -> (
    let ops = Op.ToSmt2.op_strings op in
    List.iter ops (Format.fprintf fmt "(%s ") ;
    Format.fprintf fmt "%a" term_fmt term ;
    List.iter ops (fun _ -> Format.fprintf fmt ")") ;
  )
  | Op2 (lft, op, rgt) -> (
    let ops = Op.ToSmt2.op_strings op in
    List.iter ops (Format.fprintf fmt "(%s") ;
    Format.fprintf fmt " %a" term_fmt lft ;
    Format.fprintf fmt " %a" term_fmt rgt ;
    List.iter ops (fun _ -> Format.fprintf fmt ")") ;
  )




  type typ = Int | Bool | Unk

  let merge_types t1 t2 = match t1, t2 with
  | Unk, _ -> t2
  | _, Unk -> t1
  | Int, Bool | Bool, Int ->
    (* failwith "can't merge int and bool types" *)
    Int
  | _ -> (
    assert (t1 = t2) ;
    t1
  )

  let typ_of_op op = match op with
  | Op.Plus | Op.Minus | Op.Times | Op.Div -> Int
  | Op.Neq | Op.Leq | Op.Lt | Op.Geq | Op.Gt | Op.Eq
  | Op.Not_ | Op.And_ | Op.Or_ | Op.Iff | Op.Impl | Op.ItoB -> Bool
  | _ -> Op.string_of op |> Format.sprintf "unsupported operator %s" |> failwith

  let typ_of_op_args op = match op with
  | Op.Plus | Op.Minus | Op.Times | Op.Div
  | Op.Neq | Op.Leq | Op.Lt | Op.Geq | Op.Gt | Op.ItoB -> Int
  | Op.Not_ | Op.And_ | Op.Or_ | Op.Iff | Op.Impl -> Bool
  | Op.Eq -> Unk
  | _ -> Op.string_of op |> Format.sprintf "unsupported operator %s" |> failwith

  let typ_of_simpleType ty =
    match ty with
    | SimpleType.Int_ -> Int
    | SimpleType.Bool_ -> Bool
    | _ -> Unk

  let str_of_typ typ = match typ with
  | Int -> "Int"
  | Bool -> "Bool"
  | Unk -> failwith "str_of_typ called on unknown type"

  let str_of_any_typ typ = match typ with
  | Int -> "Int"
  | Bool -> "Bool"
  | Unk -> "?"

  let typ_of_term (vars: (Objt.id * typ) list) term = match term with
  | Value (Objt.VarObj vid) -> (
    match List.find vars (fun (v, t) -> v = vid) with
    | Some (_, t) -> t
    | None -> Unk
  )
  | Value (Objt.SpecialVar vid) -> failwith "special var are not supported"
  | Value (Objt.IntObj i) -> Int
  | Value (Objt.BoolObj b) -> Bool
  | Value (Objt.Array _) -> failwith "arrays are not supported"
  | UApp (un, terms) -> Bool
  | Op1 (op, _) -> typ_of_op op
  | Op2 (_, op, _) -> typ_of_op op

  let rec collect
    ?(typ=Bool) ?(debug=false)
    (vars: (Objt.id * typ) list) (preds: UnknownPredicate.t list) term
  =
  if debug then (
    Format.printf "currently in %a (%s)@." term_fmt term (str_of_any_typ typ) ;
    (* Format.print_flush () ; *)
    (* let _ = input_line stdin in *)
    ()
  ) ;
  match term with
  | Value (Objt.VarObj vid) -> (
    (* Format.printf "var %s@." vid ; *)
    let (vars, added) = List.fold_left vars ~init:([], false) ~f:(
      fun (vars, added) (var, typ') ->
        if not added && var = vid then
          (var, merge_types typ typ') :: vars, true
        else (var, typ') :: vars, added
    ) in
    (if added then vars else (vid, typ) :: vars), preds
  )
  | Value (Objt.SpecialVar _) -> failwith "special var..."
  | Value (Objt.Array _) -> failwith "arrays are not supported..."
  | UApp (un, terms) -> (
    let preds = if List.mem ~equal:(
      fun p p' -> p.UnknownPredicate.id = p'.UnknownPredicate.id
    ) preds un then preds else un :: preds in
    List.fold_left terms ~init:(vars, preds) ~f:(
      fun (vars, preds) (term: t) ->
        collect ~debug:debug ~typ:Int vars preds term
    )
  )
  | Op1 (op, term) -> (
    assert (typ = typ_of_op op) ;
    collect ~debug:debug ~typ:(typ_of_op_args op) vars preds term
  )
  | Op2 (lft, op, rgt) -> (
    (* assert (typ = typ_of_op op) ; *)
    let arg_typ =
      merge_types (typ_of_term vars lft) (typ_of_term vars rgt)
      |> merge_types (typ_of_op_args op)
    in
    let vars, preds = collect ~debug:debug ~typ:(arg_typ) vars preds lft in
    collect ~debug:debug ~typ:(arg_typ) vars preds rgt
  )
  | _ -> (vars, preds)

  let conjuncts_of term =
    let rec loop conj = function
      | Op2 (lft, Op.And_, rgt) :: tail ->
        loop conj (lft :: rgt :: tail)
      | term :: tail ->
        loop (term :: conj) tail
      | [] -> conj
    in
    loop [] [term]

  let split_clause term =
    (* Format.printf "splitting %a@.@." term_fmt term ; *)
    let rec loop acc = function
      | Op2 (lft, Op.Impl, rgt) -> loop (acc @ (conjuncts_of lft)) rgt
      | term -> acc, List.map (conjuncts_of term) (
        function
        | Value (Objt.BoolObj false) -> None
        | rgt -> Some rgt
      )
    in
    loop [] term


  let clauses_to_smt2 fmt verbose ml_file clauses =

    Format.fprintf fmt "(set-logic HORN)@.@." ;

    (* Format.fprintf fmt "collecting predicates...@." ; *)

    let clauses = List.map clauses Boolnize.main in

    let _, preds = List.fold_left clauses ~init:([],[]) ~f:(
      fun (_, preds) clause ->
        collect ~debug:verbose [] preds clause
    ) in

    List.iter preds (
      fun pred ->
        Format.fprintf fmt "(declare-fun |%s|@.  ("
          (Identity.Short.show pred.UnknownPredicate.id) ;
        List.iter pred.UnknownPredicate.vars (
          fun _ -> Format.fprintf fmt " Int"
        ) ;
        Format.fprintf fmt " ) Bool@.)@.@."
    ) ;

    (* Format.fprintf fmt "collecting variables...@." ; *)

    List.iter clauses (
      fun clause ->
        let vars = SimpleType.Env.to_alist @@ SimpleTypeInfer.main clause in
        let lhs, rhs = split_clause clause in
        let rec loop (
          res: ((t list) * (t option list)) list
        ) mem lhs = function
          | (Some rhs) :: tail -> (
            match split_clause rhs with
            | [], rhs -> (
              match rhs with
              | [ rhs ] -> loop ( (lhs, [rhs]) :: res ) mem lhs tail
              | rhs -> loop res mem lhs (rhs @ tail)
            )
            | lhs', rhs' ->
              loop res (( (lhs @ lhs'), rhs' ) :: mem) lhs tail
          )
          | None :: tail -> (
            loop ( (lhs, [None]) :: res ) mem lhs tail
          )
          | [] -> (
            match mem with
            | [] -> res
            | (lhs, rhs) :: mem  -> loop res mem lhs rhs
          )
        in
        let split = loop [] [] lhs rhs in
        List.iter split (fun (lhs, rhs) ->
          List.iter rhs (
            fun rhs ->
              let positive = rhs <> None in
              Format.fprintf fmt "(assert@.  (" ;
              ( if positive then
                  Format.fprintf fmt "forall ("
                else
                  Format.fprintf fmt "not (exists ("
              ) ;
              ( match vars with
                | [] -> Format.fprintf fmt " (unused Int)"
                | _ -> List.iter vars (
                  fun (v, t) ->
                    try Format.fprintf fmt " (|%s| %s)" v (
                      str_of_typ @@ typ_of_simpleType t
                    )
                    with err ->
                      Format.fprintf
                        fmt "Error while getting the type of %s:@." v ;
                      raise err
                )
              ) ;
              Format.fprintf fmt " )@.    " ;
              ( if positive then
                  Format.fprintf fmt "(=>@.      "
              ) ;
              ( if lhs = [] then (
                  Format.fprintf fmt "true@."
                ) else (
                  Format.fprintf fmt "( and" ;
                  List.iter lhs (Format.fprintf fmt " %a" term_fmt) ;
                  Format.fprintf fmt " )@."
                )
              ) ;
              ( match rhs with
                | Some rhs ->
                  assert positive ;
                  Format.fprintf fmt "      %a@.    )@.  )@.)@." term_fmt rhs
                | None ->
                  Format.fprintf fmt "    )@.  )@.)@."
              )
          )
        )
        (* Format.fprintf fmt " )@.  %a@.)@.@." term_fmt rhs *)
    ) ;

    Format.fprintf fmt "(check-sat)@.@." ;

    Format.fprintf fmt "(get-model)@.@." ;

    Format.fprintf fmt "(exit)@.@."


end
