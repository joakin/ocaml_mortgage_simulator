type years = float
type year = int
type month = int
type currency = float
type interest_rate = float

module Mortgage : sig
  type t

  val encode : t -> Yojson.Basic.t
  val decode : Yojson.Basic.t -> (t, string) result
end = struct
  type rate = { first : interest_rate; rest : interest_rate }

  type t = {
    bank : string;
    rate : rate;
    extra_expenses : currency;
    amount : currency;
    years : years;
  }

  let encode m =
    `Assoc
      [
        ("bank", `String m.bank);
        ( "rate",
          `Assoc
            [ ("first", `Float m.rate.first); ("rest", `Float m.rate.rest) ] );
        ("extraExpenses", `Float m.extra_expenses);
        ("amount", `Float m.amount);
        ("years", `Float m.years);
      ]

  let decode json =
    let open Yojson.Basic.Util in
    try
      let bank = json |> member "bank" |> to_string in
      let rate =
        json |> member "rate" |> fun json ->
        {
          first = json |> member "first" |> to_float;
          rest = json |> member "rest" |> to_float;
        }
      in
      let extra_expenses = json |> member "extraExpenses" |> to_float in
      let amount = json |> member "amount" |> to_float in
      let years = json |> member "years" |> to_float in
      Ok { bank; rate; extra_expenses; amount; years }
    with exn -> Error (Printexc.to_string exn)
end
