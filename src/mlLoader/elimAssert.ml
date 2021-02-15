open! Core
open SugarProgram


let main (sprogram : t) : t =
  let mapper = { Mapper.default_mapper with
    Mapper.assert_ = (
      fun self e1 -> IfExp (self.Mapper.exp self e1, mk_true, FailExp)
    )
  } in
  expr_map sprogram ~f:(Mapper.apply_expr mapper)
