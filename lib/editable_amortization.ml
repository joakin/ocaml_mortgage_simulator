type t = Yearly of string

let from_amortization (a : Amortization.t) : t =
  match a with Yearly y -> Yearly (string_of_float y)

let to_amortization (a : t) : Amortization.t =
  match a with Yearly y -> Yearly (float_of_string y)
