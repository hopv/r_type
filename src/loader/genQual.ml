open Core
open Data

module Dict = struct
  type 'a t = (Identity.t * 'a) list

  let empty = []
  let add (dict : 'a t) ~key ~data : 'a t = (key, data) :: dict
end

let condify (exp : Program.Exp.t) : Cond.t option =
  let open Program.Exp in
  match exp with
  | Term cond -> Some cond
  | _ -> None

let to_qualifier (cond : Cond.t) (env : Cond.t Dict.t) : Cond.t =
  let cond = List.fold env ~init:cond ~f:(fun cond (vid, cond') ->
    Cond.subst_cond cond (Cond.T.(var vid), cond')
  ) in
  cond


let rec from_exp (exp : Program.Exp.t) (env : Cond.t Dict.t) : Cond.Set.t =
  let open Program.Exp in
  match exp with
  | Let_ (vid, e1, e2) ->
    let env' = Option.map (condify e1) ~f:(fun c -> Dict.add env ~key:vid ~data:c) |> Option.value ~default:env in
    from_exp e2 env'
  | Branch (es) ->
    List.fold es ~init:Cond.Set.empty ~f:(fun cs (cond, e) ->
      let new_q = to_qualifier cond env in
      Cond.Set.add cs new_q |> Cond.Set.union (from_exp e env)
    )
  | Term cond -> Cond.Set.singleton Cond.T.(var "vvvvv" == (to_qualifier cond env))
  | _ -> Cond.Set.empty


let main (program : Program.t) : Cond.Set.t =
  let exps = List.map (Program.recfuns program) ~f:(fun rf -> rf.exp) in
  List.fold exps ~init:Cond.Set.empty ~f:(fun cs exp -> Cond.Set.union cs (from_exp exp Dict.empty)) |>
  Logger.inspect ~tag:"gened qualifiers" ~f:(fun x -> (List.to_string (Cond.Set.to_list x) ~f:Cond.to_string))
