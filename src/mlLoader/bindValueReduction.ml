open Core
open SugarProgram
open Util

module Env = struct
  module Element = struct
    type t = SugarProgram.Expr.t
  end

  type t = Element.t Identity.Map.t

  let empty = Identity.Map.empty
  let find = Identity.Map.find

  let of_alist xs : t =
    Identity.Map.of_alist_reduce xs ~f:(fun a _ -> a)

  let map (self : t) ~f : t =
    Identity.Map.map self ~f
end

let main (sprogram : t) : t =
  let env = List.filter_map sprogram ~f:(fun el ->
    match el with
    | BindValue (v, exp) -> Some (v, exp)
    | _ -> None
  ) |> Env.of_alist in
  let rec replace_free_var (env : Env.t) (vars : Identity.Set.t) (exp : SugarProgram.Expr.t) : SugarProgram.Expr.t =
    let free_var_set = Identity.Set.diff (free_var_set_of exp) vars in
    Identity.Set.fold ~init:exp free_var_set ~f:(fun exp v ->
      match Env.find env v with
      | Some fv_exp ->
        let fv_exp = replace_free_var env Identity.Set.empty fv_exp in
        LetExp (v, fv_exp, exp)
      | None -> exp
    )
  in
  let recfun_names = List.filter_map sprogram ~f:(fun el -> match el with BindFunc recfun -> Some recfun.name | _ -> None) |> Identity.Set.of_list in
  let env = Env.map env ~f:(replace_free_var env Identity.Set.empty) in
  SugarProgram.map sprogram ~f:(fun el ->
    match el with
    | BindFunc recfun -> BindFunc { recfun with exp = replace_free_var env (Identity.Set.union recfun_names (Identity.Set.of_list recfun.args)) recfun.exp }
    | _ -> StructureItem.expr_map el ~f:(replace_free_var env Identity.Set.empty)
  )
