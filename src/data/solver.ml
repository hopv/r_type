open Core

let validator =
  let fail_validator (cond : Cond.t) : bool = failwith ("Cannot eval: " ^ Cond.to_string cond) in
  ref fail_validator

let validate (cond : Cond.t) : bool = !validator cond
