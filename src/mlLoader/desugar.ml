open Core

module Verification = struct
  open SugarProgram

  let rec disallow_without_simple_exp (exp : Expr.t) =
    match exp with
    | ObjExp _obj -> ()
    | SingleOpExp (_op, e) -> disallow_without_simple_exp e
    | AssertExp e -> disallow_without_simple_exp  e
    | FuncCallExp (_fid, es) -> List.iter es ~f:disallow_without_simple_exp
    | OpExp (e1, _op, e2) -> let () = disallow_without_simple_exp  e1 in disallow_without_simple_exp  e2
    | _ -> failwith ("not simple expression: " ^ Expr.to_string exp)
end

module Converter = struct
  let rec exp_to_cond (exp : SugarProgram.Expr.t) : Cond.t =
    let open SugarProgram in
    match exp with
    | ObjExp obj -> Cond.Value obj
    | SingleOpExp (op, e) -> Cond.Op1 (op, exp_to_cond e)
    | OpExp (e1, op, e2) -> Cond.Op2 (exp_to_cond e1, op, exp_to_cond e2)
    | _ -> failwith ("not allowed expression for condition: " ^ Expr.to_string exp)

  let exp_to_vid (exp : SugarProgram.Expr.t) : Identity.t =
    let open SugarProgram in
    match exp with
    | ObjExp obj when Objt.is_var obj -> Objt.vid_of_exn obj
    | _ -> failwith ("not allowed expression for condition: " ^ Expr.to_string exp)

  let sprogram_to_program (sprogram : SugarProgram.t) : Program.t =
    let open Program in
    let rec conv_exp (exp : SugarProgram.Expr.t) =
      match exp with
      | SugarProgram.LetExp (vid, e1, e2) ->
        let () = Verification.disallow_without_simple_exp e1 in
        Exp.Let_ (vid, conv_exp e1, conv_exp e2)
      | SugarProgram.BranchExp (branches) ->
        let bs = List.map branches ~f:(fun (cond_e, then_e) ->
          (exp_to_cond cond_e, conv_exp then_e)
        ) in
        Exp.Branch bs
      | SugarProgram.IfExp (e1, e2, e3) ->
        let cond = exp_to_cond e1 in
        Exp.Branch ([(cond, conv_exp e2); (Cond.DSL.not_ cond, conv_exp e3)])
      | SugarProgram.FailExp ->
        Exp.Fail
      | SugarProgram.FuncCallExp (fid, es) ->
        begin
          match es with
          | [e] -> Exp.App (fid, exp_to_vid e)
          | _ -> failwith ("unexpected expession: " ^ SugarProgram.Expr.to_string exp)
        end
      | SugarProgram.OpExp (_, _, _) ->
        Exp.Term (exp_to_cond exp)
      | SugarProgram.SingleOpExp (_, _) ->
        Exp.Term (exp_to_cond exp)
      | SugarProgram.ObjExp _ ->
        Exp.Term (exp_to_cond exp)
      | _ -> failwith ("unexpected expression: " ^ SugarProgram.Expr.to_string exp)
    in
    let from_recfun recfun : Program.Func.t =
      SugarProgram.to_pfun recfun ~exp:(conv_exp recfun.SugarProgram.exp) in
      Program.Program (
        List.map (SugarProgram.recfuns_of sprogram) ~f:from_recfun
      )
end

let main sprogram =
  sprogram
  |> ConvMain.main
  |> BindValueReduction.main
  |> ClosureConv.main
  |> AlphaConv.main
  |> ElimAssert.main
  |> KNormal.main
  |> LetNormal.main
  |> BetaReduction.main
  |> Simplify.main
  |> Converter.sprogram_to_program
