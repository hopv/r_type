open Core
open SugarProgram

let simplify_arith_exp (p : t) : t =
  let mapper = { Mapper.default_mapper with
    Mapper.op2 = (fun self (e1, op, e2) ->
      match self.Mapper.exp self e1, op, self.Mapper.exp self e2 with
      | ObjExp (Objt.IntObj 0), Op.Plus, e2' -> e2'
      | ObjExp (Objt.IntObj 0), Op.Times, e2' -> mk_int 0
      | ObjExp (Objt.IntObj 1), Op.Times, e2' -> e2'
      | e1', _, e2' -> OpExp(e1', op, e2'))
  } in
  SugarProgram.expr_map p ~f:(Mapper.apply_expr mapper)


let main p =
  simplify_arith_exp p
