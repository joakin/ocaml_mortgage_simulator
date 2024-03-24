type t = Yearly of string

let from_amortization (a : Amortization.t) : t =
  match a with Yearly y -> Yearly (string_of_float y)

let to_amortization (a : t) : Amortization.t option =
  match a with
  | Yearly y ->
      y |> float_of_string_opt |> Option.map (fun y -> Amortization.Yearly y)
