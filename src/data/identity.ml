open Core

include Mappable.Make (
    struct include String let show = ident end
)

module Short = struct
  type nonrec t = t
  let show vid =
    Label.shorten vid |> Result.ok |> Option.value ~default:vid
end

