open Core
open Sexplib.Std

type t = item list
and item = BindFunc of recfun | BindValue of Identity.t * exp | Eval of exp
and recfun = { name: Identity.t; args: args; exp: exp; annotation: Program.Func.Annotation.t }
and args = Identity.t list
and exp =
  | LetExp of Identity.t * exp * exp
  | LetRecExp of (Identity.t * exp) list * exp
  | IfExp of exp * exp * exp
  | BranchExp of (exp * exp) list
  | FailExp
  | AssertExp of exp
  | FuncCallExp of Identity.t * exp list
  | AbsExp of Identity.t list * exp
  | OpExp of exp * Op.t * exp
  | SingleOpExp of Op.t * exp
  | ObjExp of Objt.t
  [@@deriving sexp, hash, eq, ord]

module Expr = struct
  type t = exp
  [@@deriving sexp, hash, eq, ord]

  include Formatable.Make(struct
    module I = Formatable
    type nonrec t = t
    let rec to_format (self : t) =
      match self with
      | LetExp (vid, exp1, exp2) -> I.block [
            I.line ("let " ^ vid ^ " =");
            I.indent (to_format exp1);
            I.line "in";
            I.block [to_format exp2]
          ]
      | LetRecExp (binds, exp') -> I.block [
            I.line "let rec ";
            List.map binds ~f:(fun (vid, exp) ->
              I.block [
                I.line (vid ^ " =");
                I.indent (to_format exp);
              ]
            ) |> I.block |> I.indent;
            I.line "in";
            I.block [to_format exp']
          ]
      | BranchExp (branches) -> I.block [
            I.line "select";
            I.block (List.map branches ~f:(fun (exp_cond, exp_then) ->
              I.block [
                I.line "when";
                I.indent (to_format exp_cond);
                I.indent (I.block [
                  I.line "->";
                  I.indent (to_format exp_then);
                ])
              ]
            ))
          ]
      | IfExp (exp_cond, exp_then, exp_else) -> I.block [
            I.line "if";
            I.indent (to_format exp_cond);
            I.line "then";
            I.indent (to_format exp_then);
            I.line "else";
            I.indent (to_format exp_else)
          ]
      | FailExp ->
          I.text "fail"
      | AssertExp e -> I.inline [
            I.text "assert (";
            to_format e;
            I.text ")";
          ]
      | AbsExp (ids, e) ->
          I.inline [
            I.text ("\\" ^ List.to_string ids ~f:(fun x -> x));
            to_format e;
          ]
      | ObjExp obj ->
          I.text (Objt.string_of obj)
      | OpExp (e1, op, e2) -> I.inline [
            I.text "(";
            to_format e1;
            I.text (" "  ^ Op.string_of op ^ " ");
            to_format e2;
            I.text ")"
          ]
      | SingleOpExp (op, e) -> I.inline [
            I.text "(";
            I.text (Op.string_of op ^ " ");
            to_format e;
            I.text ")"
          ]
      | FuncCallExp (fid, exps) ->
          let args = List.map exps (fun exp -> to_format exp) in
          I.inline [
            I.text "(";
            I.text fid;
            I.text " ";
            I.inline (I.joint args (I.text " "));
            I.text ")"
          ]
  end)

end

module StructureItem = struct
  type t = item

  let bind_func (fp : recfun) : t = BindFunc fp
  let bind_value ((v : Identity.t), (exp : exp)) : t = BindValue (v, exp)
  let eval (exp : exp) : t = Eval exp

  let expr_map (self : t) ~f : t =
    match self with
    | BindFunc recfun -> BindFunc { recfun with exp = f recfun.exp }
    | BindValue (v, exp) -> BindValue (v, f exp)
    | Eval exp -> Eval (f exp)
end

module Func = struct
  type t = recfun

  let make ?(annotation = Program.Func.Annotation.empty) ~name ~args ~exp () =
    { name; args; exp; annotation }

  let name_of (self : t) = self.name
end

let make_recfun = Func.make

include Formatable.Make(struct
  module I = Formatable
  type nonrec t = t
  let to_format (items : t) =
    let rec from_recfun item =
      match item with
      | Eval exp -> Expr.to_format exp
      | BindValue (name, exp) -> I.block [
          I.block [
            I.text (name ^ " =")
          ];
          I.indent (Expr.to_format exp)
        ]
      | BindFunc { name; args; exp } -> I.block [
          I.block [
            I.inline (List.map (I.joint (name :: args) " ") ~f:(fun tx -> I.text tx));
            I.text " ="
          ];
          I.indent (Expr.to_format exp)
        ]
    in
    let funfmts = List.map items ~f:from_recfun in
    I.block [
      I.inline (I.joint funfmts (I.line "and"));
    ]
end)

let to_pfun { name; args; exp = exp_; annotation; } ~exp =
  Program.Func.Fields.create ~name ~args ~exp ~annotation

let recfuns_of (items : t) : Func.t list = List.map items ~f:(function BindFunc f -> Some f | _ -> None) |> List.filter_opt
(* let main_exp_of (SugarProgram (_, e)) = e *)

type sexp_and_hole = Program.Exp.t * (Program.Exp.t -> Program.Exp.t)
type obj_and_hole = Objt.t * (Program.Exp.t -> Program.Exp.t)

let id x = x
let mk_var vid = vid |> Objt.mk_var |> (fun v -> ObjExp v)
let mk_int i = i |> Objt.mk_int |> (fun v -> ObjExp v)
let mk_true = ObjExp (Objt.mk_int 1)
let mk_false = ObjExp (Objt.mk_int 0)

module L = Label.Make(struct let label = "$tmp:" end)
let gen_id = L.gen

let make_semicolon_exps exp1 exp2 = LetExp (gen_id (), exp1, exp2)

let rec free_var_set_of (self : exp) : Identity.Set.t =
  match self with
  | LetExp (vid, e1, e2) -> Identity.Set.union (free_var_set_of e1) (free_var_set_of e2) |> (fun x -> Identity.Set.remove x vid)
  | LetRecExp (binds, e2) ->
    let vids = List.map binds ~f:Tuple2.get1 |> Identity.Set.of_list in
    let set = List.fold binds ~init:(free_var_set_of e2) ~f:(fun acc (_, exp) -> Identity.Set.union acc (free_var_set_of exp)) in
    Identity.Set.diff set vids
  | BranchExp (branches) -> List.map branches ~f:(fun (ce, e) -> Identity.Set.union (free_var_set_of ce) (free_var_set_of e)) |> Identity.Set.union_list
  | IfExp (e1, e2, e3) -> Identity.Set.union (free_var_set_of e1) (free_var_set_of e2) |> Identity.Set.union (free_var_set_of e3)
  | FailExp -> Identity.Set.empty
  | AssertExp e1 -> free_var_set_of e1
  | FuncCallExp (fid, es) -> Identity.Set.add (List.map es ~f:free_var_set_of |> Identity.Set.union_list) fid
  | AbsExp (ids, e) -> Identity.Set.diff (free_var_set_of e) (Identity.Set.of_list ids)
  | OpExp (e1, op, e2) -> Identity.Set.union (free_var_set_of e1) (free_var_set_of e2)
  | SingleOpExp (op, e) -> free_var_set_of e
  | ObjExp obj -> Objt.vids_of obj |> Identity.Set.of_list

module Mapper = struct
  type t = {
    exp: t -> exp -> exp;
    let_: t -> (Identity.t * exp * exp) -> exp;
    letrec_: t -> ((Identity.t * exp) list * exp) -> exp;
    if_: t -> (exp * exp * exp) -> exp;
    branch: t -> (exp * exp) list -> exp;
    fail: t -> unit -> exp;
    assert_: t -> exp -> exp;
    funccall: t -> (Identity.t * exp list) -> exp;
    abs: t -> (Identity.t list * exp) -> exp;
    op2: t -> (exp * Op.t * exp) -> exp;
    op1: t -> (Op.t * exp) -> exp;
    obj: t -> Objt.t -> exp;
  }


  let default_mapper : t = {
    exp = (fun (self : t) (exp : exp) ->
      match exp with
      | LetExp (vid, e1, e2) ->  self.let_ self (vid, e1, e2)
      | LetRecExp (binds, e) ->  self.letrec_ self (binds, e)
      | BranchExp (branches) ->  self.branch self branches
      | IfExp (e1, e2, e3) ->    self.if_ self (e1, e2, e3)
      | FailExp ->               self.fail self ()
      | AssertExp e1 ->          self.assert_ self e1
      | FuncCallExp (fid, es) -> self.funccall self (fid, es)
      | AbsExp (ids, e) ->       self.abs self (ids, e)
      | OpExp (e1, op, e2) ->    self.op2 self (e1, op, e2)
      | SingleOpExp (op, e) ->   self.op1 self (op, e)
      | ObjExp obj ->            self.obj self obj
    );
    let_ = (fun self (vid, e1, e2) -> LetExp (vid, self.exp self e1, self.exp self e2));
    letrec_ = (fun self (binds, e2) -> LetRecExp (List.map binds ~f:(fun (vid, e) -> (vid, self.exp self e)), self.exp self e2));
    if_ = (fun self (e1, e2, e3) -> IfExp (self.exp self e1, self.exp self e2, self.exp self e3));
    branch = (fun self cs -> BranchExp (List.map cs ~f:(fun (c, e) -> (self.exp self c, self.exp self e))));
    fail = (fun _self () -> FailExp);
    assert_ = (fun self e -> AssertExp (self.exp self e));
    funccall = (fun self (fid, aes) -> FuncCallExp (fid, List.map aes ~f:(self.exp self)));
    abs = (fun self (fids, fe) -> AbsExp (fids, self.exp self fe));
    op2 = (fun self (e1, op, e2) -> OpExp (self.exp self e1, op, self.exp self e2));
    op1 = (fun self (op, e) -> SingleOpExp (op, self.exp self e));
    obj = (fun _self obj -> ObjExp obj);
  }

  let apply_expr (self : t) (exp : exp) : exp = self.exp self exp
end

let expr_map (items : t) ~f : t = List.map items ~f:(StructureItem.expr_map ~f)

let map (self : t) ~f : t = List.map self ~f
