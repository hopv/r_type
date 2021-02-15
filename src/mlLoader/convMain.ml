open Core
open SugarProgram
open! Util

module L = Label.Make (struct
  let label = "convMain"
end)

let main (sprogram : t) : t =
  List.map sprogram ~f:(fun el ->
    match el with
    | BindValue (v, e) when Stdlib.(v = "main") -> BindFunc (Func.make ~name:"main" ~args:[L.gen ()] ~exp:e ())
    | _ -> el
  )
