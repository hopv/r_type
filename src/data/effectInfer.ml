type t = raw * effect
and raw = Base | Fun of raw * t | Bot (* Bot is used only for Fail *)
and effect = Var of int | Safe | MayFail

type env = (Identity.t * raw) list

(** subtyping constraints *)
type constr =
  | TypeC of t * t
  | RawC of raw * raw
  | EffectC of effect * effect

let print_effect fm ef =
  match ef with
  | Var x -> Format.fprintf fm "?%d" x
  | Safe -> Format.fprintf fm "_"
  | MayFail -> Format.fprintf fm "!"

let rec print_raw fm raw =
  match raw with
  | Base -> Format.fprintf fm "b"
  | Fun(raw1,tmp) -> Format.fprintf fm "@[(%a ->@ %a)@]" print_raw raw1 print_template tmp
  | Bot -> Format.fprintf fm "_|_"

and print_template fm (raw,ef) =
  Format.fprintf fm "%a^%a" print_raw raw print_effect ef

let print_constr fm ef =
  match ef with
  | TypeC(tmp1,tmp2) -> Format.fprintf fm "@[%a <: %a@]" print_template tmp1 print_template tmp2
  | RawC(raw1,raw2) -> Format.fprintf fm "@[%a <: %a@]" print_raw raw1 print_raw raw2
  | EffectC(ef1,ef2) -> Format.fprintf fm "@[%a <: %a@]" print_effect ef1 print_effect ef2


module Debug = struct
  let debug = false
  let fprintf fm f = if debug then Format.fprintf fm f else Format.ifprintf fm f
  let printf fm = fprintf Format.std_formatter fm
end

let counter = ref 0
let new_var () = incr counter; !counter

let rec raw_of_ref ty =
  let open Type.RefType in
  match ty with
  | Int_ _ -> Base
  | Func(_, ty1, ty2) -> Fun(raw_of_ref ty1, template_of_ref ty2)
  | Bottom -> assert false
  | Top -> assert false
and template_of_ref ty =
  raw_of_ref ty, Var (new_var())

let rec fresh_template (raw,_) =
  let rec fresh_raw raw =
    match raw with
    | Base -> Base
    | Fun(raw1,tmp) -> Fun(fresh_raw raw1, fresh_template tmp)
    | Bot -> Bot
  in
  fresh_raw raw, Var (new_var())

let rec gen acc env e : t * constr list =
  let open Program.Exp in
  match e with
  | Let_(x, e1, e2) ->
      let (raw1,ef1),acc' = gen acc env e1 in
      let (raw2,ef2),acc'' = gen acc' ((x,raw1)::env) e2 in
      let ef = Var (new_var()) in
      (raw2,ef), EffectC(ef1,ef)::EffectC(ef2,ef)::acc''
  | Branch cs ->
      let tmps,acc' =
        let aux (_,e) (tmps,acc) =
          let tmp,acc' = gen acc env e in
          tmp::tmps, acc'
        in
        List.fold_right aux cs ([],acc)
      in
      let tmp = fresh_template @@ List.hd tmps in
      tmp, List.map (fun tmp' -> TypeC(tmp', tmp)) tmps @ acc'
  | App(f, x) ->
      let tmp11,tmp12 =
        match List.assoc f env with
        | Fun(tmp11, tmp12) -> tmp11, tmp12
        | _ -> assert false
      in
      let tmp2 = List.assoc x env in
      tmp12, RawC(tmp2, tmp11)::acc
  | Term fml when Cond.is_var fml ->
      (List.assoc (Cond.vid_exn fml) env, Safe), acc
  | Term _fml -> (Base, Safe), acc
  | Fail -> (Bot, MayFail), acc


let generate env (Program.Program fs) =
  let tmp_env = List.map (fun {Program.Func.name;_} -> name, raw_of_ref @@ Type.Env.find_exn env name) fs in
  let f {Program.Func.name; args; exp; _} acc =
    let raw = List.assoc name tmp_env in
    let tmp1,tmp_env' =
      let rec decomp acc tmp args =
        match tmp,args with
        | _, [] -> tmp, acc
        | (Fun(raw,tmp2),_), x::args' -> decomp ((x,raw)::acc) tmp2 args'
        | _ -> assert false
      in
      assert (args <> []);
      decomp tmp_env (raw,Var(new_var())) args
    in
    let tmp2,constr = gen acc tmp_env' exp in
    TypeC(tmp2,tmp1)::constr
  in
  List.fold_right f fs [], tmp_env

let rec flatten constr =
  match constr with
  | TypeC((raw1,ef1), (raw2,ef2)) -> flatten (EffectC(ef1,ef2)) @ flatten (RawC(raw1,raw2))
  | RawC(Base, Base) -> []
  | RawC(Fun(raw1,tmp1), Fun(raw2,tmp2)) -> flatten (RawC(raw2,raw1)) @ flatten (TypeC(tmp1,tmp2))
  | RawC(Bot, _) -> []
  | EffectC(Safe,_ef2) -> []
  | EffectC(ef1,ef2) -> [ef1,ef2]
  | _ -> assert false

module IntSet =
  Set.Make(
    struct
      type t = int
      let compare = compare
    end)

let solve constrs =
  let constrs' =
    let to_int ef =
      match ef with
      | Var x -> x
      | Safe -> assert false
      | MayFail -> 0
    in
    List.map (fun (x,y) -> to_int x, to_int y) constrs
  in
  let upper =
    let n = !counter + 1 in
    let a = Array.make n [] in
    List.iter (fun (x,y) -> a.(x) <- y::a.(x)) constrs';
    a
  in
  let rec solve may_fail rest =
    Debug.printf "|rest|: %d@." (List.length rest);
    match rest with
    | [] -> may_fail
    | x::rest' ->
        let up = List.filter (fun y -> not @@ IntSet.mem y may_fail) upper.(x) in
        List.iter (Debug.printf "%d ADDED@.") up;
        let may_fail' = List.fold_right IntSet.add up may_fail in
        solve may_fail' (up@rest')
  in
  let may_fail = solve IntSet.empty [0] in
  fun x ->
    if IntSet.mem x may_fail then
      MayFail
    else
      Safe

let apply_sol sol env =
  let rec to_raw raw =
    match raw with
    | Base -> Base
    | Fun(raw,tmp2) -> Fun(to_raw raw, to_tmp tmp2)
    | Bot -> Bot
  and to_tmp (raw,ef) =
    let ef' =
      match ef with
      | Var x -> sol x
      | Safe -> Safe
      | MayFail -> MayFail
    in
    to_raw raw, ef'
  in
  List.map (fun (f,raw) -> f, to_raw raw) env

let infer rtenv prog : env =
  counter := 0;
  Debug.printf "prog: %s@." @@ Program.to_string prog;
  let constrs,env = generate rtenv prog in
  Debug.printf "@.@.TEMPLATES:@.";
  List.iter (fun (x,raw) -> Debug.printf "%s: %a@." x print_raw raw) env;
  Debug.printf "@.@.CONSTRS:@.";
  List.iter (Debug.printf "  %a@." print_constr) constrs;
  let constrs = List.flatten @@ List.map flatten constrs in
  Debug.printf "@.@.FLATTEN:@.";
  List.iter (fun (ef1,ef2) -> Debug.printf "  %a <: %a@." print_effect ef1 print_effect ef2) constrs;
  let sol = solve constrs in
  let env' = apply_sol sol env in
  Debug.printf "@.@.INFERRED:@.";
  List.iter (fun (x,raw) -> Debug.printf "%s: %a@." x print_raw raw) env';
  Debug.printf "@.@.RTENV: %s@." (Type.Env.to_string rtenv);
  env'


let assumed_as_true rtenv prog =
  let module RT = Type.RefType in
  let env = infer rtenv prog in
  let rec aux raw ty =
    match raw, ty with
    | Base, _ -> []
    | Fun(raw1,tmp2), RT.Func(_,ty1,ty2) ->
        let ps1 = aux raw1 ty1 in
        let ps2 = aux (fst tmp2) ty2 in
        let ps3 =
          match ty1 with
          | RT.Int_(_, Cond.UApp(p,_)) ->
              let rec next_effect tmp2 =
                match tmp2 with
                | _, MayFail -> MayFail
                | Fun(Fun _,tmp22), _ -> next_effect tmp22
                | _, ef -> ef
              in
              if next_effect tmp2 = Safe then
                let x = UnknownPredicate.id_of p in
                let () = Debug.printf "P: %s@." x in
                [x]
              else
                []
          | _ -> []
        in
        ps3 @ ps1 @ ps2
    | Fun _, _ -> assert false
    | Bot, _ -> assert false
  in
  List.flatten @@ List.map (fun (f,raw) -> aux raw @@ Type.Env.find_exn rtenv f) env
