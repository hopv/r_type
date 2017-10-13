open Core
open SugarProgram

module V = Label.Make(struct let label = "knormal" end)

module Context = struct
  type 'a t = 'a -> Expr.t

  let insert_let (exp : Expr.t) (context : Expr.t t) : Expr.t =
    match exp with
    | ObjExp objt -> context exp
    | _ ->
      let vid = V.gen () in
      let obj_exp = ObjExp (Objt.mk_var vid) in
      LetExp (vid, exp, context obj_exp)

  let insert_let_unless_var (exp : Expr.t) (var_context : Identity.t t) : Expr.t =
    match exp with
    | ObjExp objt when Objt.is_var objt -> var_context (Objt.vid_of_exn objt)
    | _ ->
      let vid = V.gen () in
      LetExp (vid, exp, var_context vid)
end

let knormal_mapper : Mapper.t =
  { Mapper.default_mapper with
    Mapper.op2 = (fun self (e1, op, e2) ->
      Context.insert_let (self.Mapper.exp self e1) (fun x ->
        Context.insert_let (self.Mapper.exp self e2) (fun y ->
          OpExp (x, op, y)
        )
      )
    );
    Mapper.op1 = (fun self (op, e) ->
      Context.insert_let (self.Mapper.exp self e) (fun x ->
        SingleOpExp (op, x)
      )
    );
    Mapper.funccall = (fun self (fid, exps) ->
      let exps = List.map exps ~f:(self.Mapper.exp self) in
      List.fold exps ~init:(ObjExp (Objt.mk_var fid)) ~f:(fun fe x ->
        Context.insert_let_unless_var x (fun v ->
          Context.insert_let_unless_var fe (fun fid ->
            Context.insert_let (FuncCallExp (fid, [ObjExp (Objt.mk_var v)])) (fun x -> x)
          )
        )
      )
    );
    Mapper.if_ = (fun self (cond_e, then_e, else_e) ->
      Context.insert_let (self.Mapper.exp self cond_e) (fun e ->
        IfExp (e, self.Mapper.exp self then_e, self.Mapper.exp self else_e)
      )
    );
    Mapper.assert_ = (fun self e ->
      Context.insert_let (self.Mapper.exp self e) (fun e ->
        AssertExp e
      )
    );
  }

let main (sprogram : t) : t =
  SugarProgram.expr_map sprogram ~f:(Mapper.apply_expr knormal_mapper)
