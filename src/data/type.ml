open Core

module RefType = struct
  type t =
      Func of Identity.t * t * t
    | Int_ of Identity.t * Cond.t
    | Bottom
    | Top
    [@@deriving sexp, variants, eq, ord, hash]

  module T = struct
    let int_ x y = Int_ (x, y)
  end

  module L = Label.Make(struct let label = "V-reftype" end)

  let is_base = function Int_ (_, _) -> true | _ -> false
  let rec subst self orig_var new_var =
    match self with
    | Int_ (v, cond) ->
        if v = orig_var
        then Int_ (new_var, Cond.subst cond orig_var new_var)
        else Int_ (v, Cond.subst cond orig_var new_var)
    | Func (v, t1, t2) ->
        if v = orig_var
        then Func (new_var, subst t1 orig_var new_var, subst t2 orig_var new_var)
        else Func (v, subst t1 orig_var new_var, subst t2 orig_var new_var)
    | _ -> self

  let subst_nu self new_var =
    match self with
    | Int_ (nu, _) -> subst self nu new_var
    | Func (nu, _, _) -> subst self nu new_var
    | _ -> self

  let rec fresh self =
    match self with
    | Int_ (v, cond) ->
        let fresh_id = L.gen () in
        subst (Int_ (v, cond)) v fresh_id
    | Func (v, t1, t2) ->
        let fresh_id = L.gen () in
        subst (Func (v, fresh t1, fresh t2)) v fresh_id
    | _ -> self

  let rec assign_unknown (self : t) (tsubst : Cond.UnknownSubst.t) : t =
    match self with
    | Int_ (v, cond) ->
        Int_ (v, Cond.UnknownSubst.assign tsubst cond)
    | Func (v, t1, t2) ->
      Func (v, assign_unknown t1 tsubst, assign_unknown t2 tsubst)
    | _ -> failwith "unexpected"

  let rec assign_obj (self : t) (var : Identity.t) (obj : Objt.t) : t =
    match self with
    | Int_ (v, cond') ->
      Int_ (v, Cond.assign_obj cond' var obj)
    | Func (v, ty1, ty2) ->
      Func (v, assign_obj ty1 var obj, assign_obj ty2 var obj)
    | x -> x

  let assign_nu (self : t) (obj : Objt.t) : t =
    match self with
    | Int_ (v, _) ->
      assign_obj self v obj
    | Func (v, _, _) ->
      assign_obj self v obj
    | x -> x

  let uapp_of (self : t) : Cond.UnknownApp.t option =
    match self with
    | Int_ (v, cond) -> Cond.uapp_of cond
    | _ -> None

  let rec uapps_of (self : t) : Cond.UnknownApp.t list =
    match self with
    | Int_ (v, cond) -> Cond.uapps_of cond
    | Func (v, ty1, ty2) -> uapps_of ty1 @ uapps_of ty2
    | _ -> failwith "unexpected"

  let name_of (self : t) =
    match self with
    | Int_ (nu, _) -> nu
    | Func (nu, _, _) -> nu
    | _ -> failwiths "unexpected" self sexp_of_t

  let arg = function
  | Func (vid, t1, t2) -> t1
  | _ -> failwith "unexpected"
  let rtn = function
  | Func (vid, t1, t2) -> t2
  | _ -> failwith "unexpected"
  let vid = function
  | Func (vid, t1, t2) -> vid
  | _ -> failwith "unexpected"

  let rec argument_types (self : t) =
    match self with
    | Func (_, t1, t2) -> t1 :: argument_types t2
    | _ -> []

  let rec id_types_for_func (self : t) len =
    if len = 0 then
      []
    else
      match self with
      | Func (id, t1, t2) -> (id, t1) :: id_types_for_func t2 (len - 1)
      | Int_ (id, _) -> [(id, self)]
      | _ -> []

  module D = struct
    let denote x t =
      match t with
      | Int_ (v, cond') ->
          Cond.subst cond' v x
      | Func _ ->
          Cond.true_
      | _ -> failwith "unexpected"
  end
  include D

  let from_condition ~f =
    let new_vid = L.gen () in int_ new_vid (f new_vid)

  include Formatable.Make(struct
    module I = Formatable

    type nonrec t = t
    let rec to_format self =
      match self with
      | Func (vid, t1, t2) ->
          I.inline [
            I.text ("( " ^ Identity.Short.show vid ^ " : ");
            to_format t1;
            I.text " -> ";
            to_format t2;
            I.text ")";
          ]
      | Int_ (vid, c1) ->
          I.inline [
            I.text "{ ";
            I.text (Identity.Short.show vid);
            I.text " : int | ";
            Cond.Simplifier.to_format c1;
            I.text " }";
          ]
      | Bottom ->
          I.noline "bottom";
      | Top ->
          I.noline "top";
  end)

  module Ord = struct
    type nonrec t = t * t

    let rec denote = function
      | (Top, _) -> Cond.true_
      | (_, Top) -> Cond.false_
      | (Int_ (v, cond1), Int_ (w, cond2)) ->
          Cond.T.(cond1 ==> Cond.subst cond2 w v)
      | (Func (v, t1, t2), Func (w, t1', t2')) ->
          Cond.T.(denote (subst t1' w v, t1) && (D.denote v t1' ==> denote (t2, subst t2' w v)))
      | (e1, e2) -> failwith "illigal pattern"
  end

  module FromSimpleType = struct
    let refine ?(prefix = "") ?(main = false) stype =
      let rec f ~truthy variables = function
      | SimpleType.Func (t1, t2) ->
          let rt1 = f ~truthy variables t1 in
          let new_id = L.gen () in
          let new_variables = if SimpleType.is_simple t1 then (new_id :: variables) else variables in
          let rt2 = f ~truthy:(truthy (* && not (SimpleType.is_simple t2) *) ) new_variables t2 in
          Func (new_id, rt1, rt2)
      | SimpleType.Int_ ->
          let new_id = L.gen () in
          let cond =
            if truthy then Cond.true_
            else Cond.UnknownApp.from_predicate (UnknownPredicate.init ~prefix (new_id :: variables)) |> Cond.UnknownApp.to_cond in
          Int_ (new_id, cond)
      | _ -> failwith "unexpected"
      in f ~truthy:main [] stype
  end

  let rec conds_of = function
    | Int_ (_, cond) -> [cond]
    | Func (_, t1, t2) -> conds_of t1 @ conds_of t2
    | _ -> []

  let cond_of = function
    | Int_ (_, cond) -> Some cond
    | _ -> None
end

module Env = struct
  module Map = Identity.Map
  module Element = struct
    type t = Condition of Cond.t | Mapping of Identity.t * RefType.t
    [@@deriving sexp]
  end

  type t = RefType.t Map.t * (Cond.t list)

  let empty = (Map.empty, [])
  let denote ((maps, cs) : t) : Cond.t =
    let var_types = Map.to_alist maps in
    Cond.T.(List.fold var_types ~init:Cond.true_ ~f:(fun acc (x, t) -> Cond.T.(acc && RefType.denote x t)) && Cond.forall cs ident)

  let find_exn (maps, _) key =
    Map.find_exn maps key

  let ty_map (mp, cs) ~f =
    (Map.map mp ~f, cs)

  let types_of (mp, _) = Map.data mp

  module T = struct
    let (@<<) (map, cs) = function
      | Element.Condition cond -> (map, cond :: cs)
      | Element.Mapping (key, data) -> (Map.add map ~key ~data, cs)

    let from_condition cond = Element.Condition cond
    let from_map (k, v) = Element.Mapping (k, v)
  end

  let assign_unknown (mp, conds) tsubst =
    (Map.map mp ~f:(fun ty -> RefType.assign_unknown ty tsubst), conds)

  let from_simple_type_env simptyenv =
    SimpleType.Env.fold simptyenv ~init:empty ~f:(fun ~key ~data tyenv ->
      T.(tyenv @<< from_map (key, (RefType.FromSimpleType.refine ~prefix:key ~main:(key = "main") data)))
    )

  let to_string (mp, conds) =
    "\n" ^ List.fold (Map.to_alist mp) ~init:"" ~f:(fun str (key, data) ->
      key ^ ": " ^ RefType.to_string data ^ "\n" ^ str) ^ "\n" ^
      List.fold conds ~init:"" ~f:(fun str cond -> Cond.to_string cond ^ str)
end

module Extended = struct
  type t =
      And_ of t * t
    | Impl of Cond.t * RefType.t
    [@@deriving sexp, variants]


  module Ord = struct
    type nonrec t = t * RefType.t

    let rec denote tyenv = function
      | (And_ (ut1, ut2), t) ->
          Cond.T.(denote tyenv (ut1, t) && denote tyenv (ut2, t))
      | (Impl (cond, t1), t2) ->
          Cond.T.(Env.denote tyenv ==> (cond ==> RefType.Ord.denote (t1, t2)))
  end

  module And_Plus = struct
    let rec fold ~init ~impl t =
      let (%%) acc t' = fold t' ~init:acc ~impl in
      let and_ v t1 t2 = init %% t1 %% t2 in
      let impl v cond reftype = impl init cond reftype in
      Variants.map t ~and_ ~impl

    let map_to_cond ~f t =
      let impl acc cond reftype = Cond.T.(acc && f cond reftype) in
      fold ~init:Cond.true_ ~impl t

    let rec map ~f t =
      let dig t = map ~f t in
      let and_ v t1 t2 = and_ (dig t1) (dig t2) in
      let impl v cond reftype = f cond reftype in
      Variants.map t ~and_ ~impl
  end

  let to_ref = function
    | Impl (_, ext) -> ext
    | _ -> failwith "unexpected"
  let rec denote x = function
    | And_ (ut1, ut2) ->
        Cond.T.(denote x ut1 || denote x ut2)
    | Impl (cond, t) ->
        Cond.T.(cond ==> RefType.denote x t)

  let lift reftype =
    Impl (Cond.true_, reftype)
  let fall = function
  | Impl (_, reftype) -> reftype
  | _ -> failwith "unexpected"

  let multiple_impl cond t  =
    let f cond' reftype = impl Cond.T.(cond && cond') reftype in
    And_Plus.map t ~f

  let rec subst self orig_var new_var =
    match self with
    | And_ (ut1, ut2) ->
        And_ (subst ut1 orig_var new_var, subst ut2 orig_var new_var)
    | Impl (cond, t) ->
        Impl (Cond.subst cond orig_var new_var, RefType.subst t orig_var new_var)

  let rec subst_nu self new_var =
    match self with
    | And_ (ut1, ut2) ->
        And_ (subst_nu ut1 new_var, subst_nu ut2 new_var)
    | Impl (cond, t) ->
        Impl (cond, RefType.subst_nu t new_var)

  module T = struct
    let (&&) x y = And_ (x, y)
    let (==>) x y = Impl (x, y)
    let (==>&) x y = multiple_impl x y
  end

  let rec assign_unknown (self : t) tsubst =
    match self with
      | And_ (ut1, ut2) ->
          And_ (assign_unknown ut1 tsubst, assign_unknown ut2 tsubst)
      | Impl (cond, t1) ->
          Impl (Cond.UnknownSubst.assign tsubst cond, RefType.assign_unknown t1 tsubst)

  include Formatable.Make(struct
    module I = Formatable

    type nonrec t = t
    let rec to_format self =
      match self with
      | And_ (t1, t2) ->
          I.inline [
            I.text "(";
            to_format t1;
            I.text " & ";
            to_format t2;
            I.text ")";
          ]
      | Impl (c1, t1) ->
          I.inline [
            I.text "(";
            Cond.Simplifier.to_format c1;
            I.text " ==> ";
            RefType.to_format t1;
            I.text ")";
          ]
  end)
end
include Extended
