open Core
open SugarProgram

let letnormal_mapper : Mapper.t =
  let open Mapper in
  { default_mapper with
    let_ = (fun self (vid, e1, e2) ->
      let rec anormal exp =
        match exp with
        | IfExp (ce, te, ee) ->
          IfExp (self.exp self ce, anormal te, anormal ee)
        | LetExp (vid', e1', e2') ->
          LetExp (vid', e1', anormal e2')
        | BranchExp (bs) ->
          BranchExp (List.map bs ~f:(fun (ce, te) -> (self.exp self ce, anormal te)))
        | FailExp -> FailExp
        | _ -> LetExp (vid, exp, self.exp self e2)
      in anormal (self.exp self e1)
    );
  }

let main (sprogram : t) : t =
  SugarProgram.expr_map sprogram ~f:(Mapper.apply_expr letnormal_mapper)
