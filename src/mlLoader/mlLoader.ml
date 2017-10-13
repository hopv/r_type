open Core

module L = Label.Make(struct
  let label = "mlloader"
end)

module P = struct
  open Asttypes
  open Parsetree
  open SugarProgram

  type t = {
    structure: t -> structure -> SugarProgram.t;
    structure_item: t -> structure_item -> StructureItem.t list;
    value_binding: t -> value_binding -> (Identity.t * Expr.t) list;
    expr: t -> expression -> Expr.t;
    construct: t -> (Longident.t loc * expression option) -> Expr.t;
    apply: t -> expression -> expression list -> Expr.t;
    constant: t -> constant -> Objt.t;
    bind_pattern: t -> Expr.t -> pattern -> (Identity.t * Expr.t) list;
  } [@@deriving fields]

  let p_structure (self : t) (st : structure) : SugarProgram.t =
    List.map st ~f:(self.structure_item self) |> List.concat

  let rec pile_multi_abs (expr : Expr.t) : Expr.t =
    match expr with
    | AbsExp (vids, fun_expr) ->
      begin
        match pile_multi_abs fun_expr with
        | AbsExp (vids', fun_expr') -> AbsExp (vids @ vids', fun_expr')
        | fun_expr -> AbsExp (vids, fun_expr)
      end
    | _ -> expr

  let p_structure_item (self : t) ({ pstr_loc = _loc; pstr_desc = desc } : structure_item) : StructureItem.t list =
    match desc with
    | Pstr_eval (expr, _attr) -> [StructureItem.eval (self.expr self expr)]
    | Pstr_value (_is_recursive, vbs) ->
      List.map vbs ~f:(self.value_binding self) |> List.concat |> List.map ~f:(fun (vid, expr) ->
        match pile_multi_abs expr with
        | AbsExp (vids, fun_expr) -> Some (SugarProgram.Func.make ~name:vid ~args:vids ~exp:fun_expr () |> SugarProgram.StructureItem.bind_func)
        | expr -> Some (SugarProgram.StructureItem.bind_value (vid, expr))
      ) |> List.filter_opt
    | _ -> []

  let p_value_binding (self : t) ({ pvb_pat = pat; pvb_expr = expr; pvb_attributes = _attrs; pvb_loc = _loc } : value_binding) =
    let sp_expr = self.expr self expr in self.bind_pattern self sp_expr pat

  let p_expr (self : t) ({pexp_loc = loc; pexp_desc = desc; pexp_attributes = _attrs}: expression) : Expr.t =
    match desc with
    | Pexp_fun (alabel, defe, fun_pat, fun_expr) ->
      let () =
        begin
          match (alabel, defe) with
          | (Nolabel, None) -> ()
          | _ -> failwith "unsupported syntax"
        end
      in
      begin
        match self.bind_pattern self (self.expr self fun_expr) fun_pat with
        | [(vid, exp)] -> pile_multi_abs (AbsExp ([vid], exp))
        | _ -> failwith "unsupported syntax"
      end
    | Pexp_let (rec_flag, value_bindings, cont_expr) ->
      let bindings = List.map value_bindings ~f:(fun x -> self.value_binding self x) |> List.concat in
      begin
        match rec_flag with
        | Asttypes.Recursive -> LetRecExp (bindings, self.expr self cont_expr)
        | _ -> List.fold_right bindings ~init:(self.expr self cont_expr) ~f:(fun (vid, expr) cont -> LetExp (vid, expr, cont))
      end
    | Pexp_apply (fexp, alabels) ->
      self.apply self fexp (List.map alabels ~f:(fun (_, x) -> x))
    | Pexp_ifthenelse (cond_exp, then_exp, else_exp_opt) ->
      let else_exp = (match else_exp_opt with Some e -> self.expr self e | None -> mk_true) in
      IfExp (self.expr self cond_exp, self.expr self then_exp, else_exp)
    | Pexp_sequence (exp1, exp2) ->
      let vid = L.gen () in
      LetExp (vid, self.expr self exp1, self.expr self exp2)
    | Pexp_constraint (expr, _type) -> self.expr self expr
    | Pexp_assert expr -> AssertExp (self.expr self expr)
    | Pexp_constant const -> ObjExp (self.constant self const)
    | Pexp_ident { txt = ident; loc = _loc; } -> ObjExp (Objt.varobj (Longident.last ident))
    | Pexp_construct (loct, exp) -> self.construct self (loct, exp)
    | _ -> failwith "unsupported expr"

  let p_construct (self : t) ({ txt = ident; loc = loc; }, exp) : Expr.t =
    match (Longident.last ident, exp) with
    | ("()", None) -> mk_true
    | ("false", None) -> mk_false
    | ("true", None) -> mk_true
    | _ -> failwith "unsupported construction"

  let p_apply (self : t) (f_expr : expression) (a_exprs : expression list) : Expr.t =
    let sp_expr = self.expr self f_expr in
    let arg_exprs = List.map a_exprs ~f:(self.expr self) in
    match sp_expr with
    | ObjExp ob when Objt.is_var ob ->
      let vid = Objt.vid_of_exn ob in
      begin
        match (Op.of_string vid, arg_exprs) with
        | (Some (Op.Minus as op), [t1]) -> SingleOpExp (op, t1)
        | (Some (Op.Not_ as op), [t1]) -> SingleOpExp (op, t1)
        | (Some op, [t1; t2]) -> OpExp (t1, op, t2)
        | (Some op, _) -> failwith "unsupported operator"
        | (_, _) -> FuncCallExp (vid, arg_exprs)
      end
    | _ ->
      let vid = L.gen () in
      LetExp (vid, sp_expr, FuncCallExp (vid, arg_exprs))

  let p_constant (self : t) (const : constant) : Objt.t =
    match const with
    | Pconst_integer (int_str, _suffix) -> Objt.intobj (Int.of_string int_str)
    | Pconst_char c -> Objt.varobj (String.of_char c)
    | Pconst_string (str, _) -> Objt.varobj str
    | _ -> failwith "unsupported constant"

  let p_bind_pattern (self : t) (e : Expr.t) ({ ppat_desc = pat; ppat_loc = loc; ppat_attributes = _attrs } : pattern) : (Identity.t * Expr.t) list =
    match pat with
    | Ppat_any -> [(L.gen (), e)]
    | Ppat_var ({ txt = vid; loc = _loc; }) -> [(vid, e)]
    | Ppat_constraint (pat, _type) -> self.bind_pattern self e pat
    | _ -> failwith "unsupported pattern"

  let converter : t = {
    structure = p_structure;
    structure_item = p_structure_item;
    value_binding = p_value_binding;
    expr = p_expr;
    construct = p_construct;
    apply = p_apply;
    constant = p_constant;
    bind_pattern = p_bind_pattern;
  }

  let convert (self : t) (str : structure) : SugarProgram.t = self.structure self str
end

let parse filename : SugarProgram.t =
  let structure = Pparse.parse_implementation ~tool_name:"fpice" Format.std_formatter filename in
  P.convert P.converter structure

let desugar (sprogram : SugarProgram.t) : Program.t = Desugar.main sprogram
