open Core
open SugarProgram

module Env = struct
  type t = (Identity.t * Identity.t) list

  let empty = []

  let add (self : t) (key : Identity.t) (data : Identity.t) : t = (key, data) :: self

  let rec find (self : t) (key : Identity.t) : Identity.t option =
    match self with
    | [] -> None
    | (k, v) :: self when k = key -> Some (Option.value (find self v) ~default:v)
    | _ :: self -> find self key
end

let rec mapper_fn (env : Env.t) : Mapper.t =
  { Mapper.default_mapper with
    Mapper.let_ = (fun self (vid, e1, e2) ->
      match self.Mapper.exp self e1 with
      | ObjExp objt when Objt.is_var objt ->
        let env = Env.add env vid (Objt.vid_of_exn objt) in
        Mapper.apply_expr (mapper_fn env) e2
      | e1 -> LetExp (vid, e1, self.Mapper.exp self e2)
    );
    Mapper.funccall = (fun self (fid, exps) ->
      let exps = List.map exps ~f:(self.Mapper.exp self) in
      match Env.find env fid with
      | None -> FuncCallExp (fid, exps)
      | Some x -> FuncCallExp (x, exps)
    );
    Mapper.obj = (fun self objt ->
      match (Objt.vid_of objt |> (fun x -> Option.bind x (Env.find env))) with
      | None -> ObjExp objt
      | Some x -> ObjExp (Objt.mk_var x)
    );
  }

let main (sprogram : t) : t =
  SugarProgram.expr_map sprogram ~f:(Mapper.apply_expr (mapper_fn Env.empty))
