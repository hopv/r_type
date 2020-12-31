open Core
open SugarProgram

module AssignMap = Map.Make(String)

let (<*>) a1 a2 = List.zip_exn a1 a2

class ['a] scope =
  object (self)
    val mutable vmap = (AssignMap.empty : 'a AssignMap.t)
    method get = vmap
    method set nmap = vmap <- nmap
    method add vkey vid =
      match AssignMap.add vmap vkey vid with
      | `Ok vmap' -> vmap <- vmap'
      | `Duplicate -> assert false
    method mem vkey = AssignMap.mem vmap vkey
    method find vkey = AssignMap.find vmap vkey
  end

class flesh_var scope =
  object (self)
    val mutable id = 0
    val current = scope
    method private intro vid =
      id <- id + 1;
      Label.alpha_vid id vid
    method conv vid =
      let rec loop aid =
        if current#mem aid then loop (self#intro aid) else (current#add vid aid; aid)
      in loop (self#intro vid)
  end

let main (sprogram : SugarProgram.t) : SugarProgram.t =
  let scope = new scope in
  let nest f =
    let backup = scope#get in
    let res = f () in
    scope#set backup; res
  in
  let alpha = new flesh_var scope in

  let conv_vid vid =
    match scope#find vid with
    | Some v -> v
    | None -> vid (* (Label.freevar_vid vid) *) in
  let mapper : Mapper.t = { Mapper.default_mapper with
      Mapper.abs = (fun self (vids, exp) ->
        nest (fun () ->
          let vids' = List.map vids ~f:(fun v -> alpha#conv v) in
          AbsExp (vids', self.Mapper.exp self exp)
        )
      );
      Mapper.let_ = (fun self (vid, exp1, exp2) ->
        let exp1' = self.Mapper.exp self exp1 in
        nest (fun () ->
          let alpha_vid = alpha#conv vid in
          LetExp (alpha_vid, exp1', self.Mapper.exp self exp2)
        )
      );
      Mapper.funccall = (fun self (fid, exps) ->
        FuncCallExp (conv_vid fid, List.map exps ~f:(self.Mapper.exp self))
      );
      Mapper.obj = (fun self (obj) ->
        if Objt.is_var obj
        then
          mk_var (conv_vid (Objt.vid_of_exn obj))
        else
          ObjExp obj
      );
    }
  in
  SugarProgram.map sprogram ~f:(fun el ->
    match el with
    | BindFunc recfun ->
      nest (fun () ->
        let alpha_args = List.map recfun.args ~f:(fun x -> alpha#conv x) in
        let exp = Mapper.apply_expr mapper recfun.SugarProgram.exp in
        BindFunc { recfun with exp = exp; args = alpha_args; }
      )
    | _ -> StructureItem.expr_map el ~f:(Mapper.apply_expr mapper)
  )
