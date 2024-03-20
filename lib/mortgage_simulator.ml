type years = float
type year = int
type month = int
type interest_rate = float

let unwrap = function Ok x -> x | Error e -> failwith e

module Currency : sig
  type t

  val encode : t -> Yojson.Basic.t
  val decode : Yojson.Basic.t -> (t, string) result
end = struct
  type t = float

  let encode c = `Float c

  let decode json =
    let open Yojson.Basic.Util in
    try Ok (json |> to_float) with exn -> Error (Printexc.to_string exn)
end

module Mortgage : sig
  type t

  val encode : t -> Yojson.Basic.t
  val decode : Yojson.Basic.t -> (t, string) result
end = struct
  type rate = { first : interest_rate; rest : interest_rate }

  type t = {
    bank : string;
    rate : rate;
    extra_expenses : Currency.t;
    amount : Currency.t;
    years : years;
  }

  let encode m =
    `Assoc
      [
        ("bank", `String m.bank);
        ( "rate",
          `Assoc
            [ ("first", `Float m.rate.first); ("rest", `Float m.rate.rest) ] );
        ("extraExpenses", Currency.encode m.extra_expenses);
        ("amount", Currency.encode m.amount);
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
      let extra_expenses =
        json |> member "extraExpenses" |> Currency.decode |> unwrap
      in
      let amount = json |> member "amount" |> Currency.decode |> unwrap in
      let years = json |> member "years" |> to_float in
      Ok { bank; rate; extra_expenses; amount; years }
    with exn -> Error (Printexc.to_string exn)
end

module EditableMortgage = struct
  type rate = { first : string; rest : string }

  type t = {
    bank : string;
    rate : rate;
    extra_expenses : string;
    amount : string;
    years : string;
  }

  let empty =
    {
      bank = "";
      rate = { first = "1"; rest = "1" };
      extra_expenses = "0";
      amount = "100000";
      years = "20";
    }
end

module Amortization : sig
  type t

  val encode : t -> Yojson.Basic.t
  val decode : Yojson.Basic.t -> (t, string) result
end = struct
  type t = Yearly of Currency.t

  let encode (Yearly y) =
    `Assoc [ ("kind", `String "Yearly"); ("value", Currency.encode y) ]

  let decode json =
    let open Yojson.Basic.Util in
    try
      let kind = json |> member "kind" |> to_string in
      match kind with
      | "Yearly" ->
          Ok (Yearly (json |> member "value" |> Currency.decode |> unwrap))
      | _ -> Error (Printf.sprintf "Invalid kind: %s" kind)
    with exn -> Error (Printexc.to_string exn)
end

module EditableAmortization = struct
  type t = Yearly of string
end

module MonthlyRecord : sig
  type t

  val encode : t -> Yojson.Basic.t
  val decode : Yojson.Basic.t -> (t, string) result
end = struct
  type t = {
    year : year;
    month : month;
    monthly_payment : Currency.t;
    interest_payed : Currency.t;
    amortized : Currency.t;
    extra_amortization : Currency.t;
    remaining_loan : Currency.t;
  }

  let encode m =
    `Assoc
      [
        ("year", `Int m.year);
        ("month", `Int m.month);
        ("monthlyPayment", Currency.encode m.monthly_payment);
        ("interestPayed", Currency.encode m.interest_payed);
        ("amortized", Currency.encode m.amortized);
        ("extraAmortization", Currency.encode m.extra_amortization);
        ("remainingLoan", Currency.encode m.remaining_loan);
      ]

  let decode json =
    let open Yojson.Basic.Util in
    try
      let year = json |> member "year" |> to_int in
      let month = json |> member "month" |> to_int in
      let monthly_payment =
        json |> member "monthlyPayment" |> Currency.decode |> unwrap
      in
      let interest_payed =
        json |> member "interestPayed" |> Currency.decode |> unwrap
      in
      let amortized = json |> member "amortized" |> Currency.decode |> unwrap in
      let extra_amortization =
        json |> member "extraAmortization" |> Currency.decode |> unwrap
      in
      let remaining_loan =
        json |> member "remainingLoan" |> Currency.decode |> unwrap
      in
      Ok
        {
          year;
          month;
          monthly_payment;
          interest_payed;
          amortized;
          extra_amortization;
          remaining_loan;
        }
    with exn -> Error (Printexc.to_string exn)
end

module Results = struct
  type monthly_payment = { first : Currency.t; rest : Currency.t }

  type t = {
    monthly_payment : monthly_payment;
    finishes_paying_in : years;
    total_interests : Currency.t;
    total_expenses_and_interests : Currency.t;
    records : MonthlyRecord.t list;
  }

  let encode r =
    `Assoc
      [
        ( "monthly_payment",
          `Assoc
            [
              ("first", Currency.encode r.monthly_payment.first);
              ("rest", Currency.encode r.monthly_payment.rest);
            ] );
        ("finishes_paying_in", `Float r.finishes_paying_in);
        ("total_interests", Currency.encode r.total_interests);
        ( "total_expenses_and_interests",
          Currency.encode r.total_expenses_and_interests );
        ("records", `List (List.map MonthlyRecord.encode r.records));
      ]

  let decode json =
    let open Yojson.Basic.Util in
    try
      let monthly_payment =
        json |> member "monthly_payment" |> fun json ->
        {
          first = json |> member "first" |> Currency.decode |> unwrap;
          rest = json |> member "rest" |> Currency.decode |> unwrap;
        }
      in
      let finishes_paying_in =
        json |> member "finishes_paying_in" |> to_float
      in
      let total_interests =
        json |> member "total_interests" |> Currency.decode |> unwrap
      in
      let total_expenses_and_interests =
        json
        |> member "total_expenses_and_interests"
        |> Currency.decode |> unwrap
      in
      let records =
        json |> member "records" |> to_list
        |> List.map (fun json -> json |> MonthlyRecord.decode |> unwrap)
      in
      Ok
        {
          monthly_payment;
          finishes_paying_in;
          total_interests;
          total_expenses_and_interests;
          records;
        }
    with exn -> Error (Printexc.to_string exn)
end

module Report : sig
  type t

  val encode : t -> Yojson.Basic.t
  val decode : Yojson.Basic.t -> (t, string) result
end = struct
  type t = {
    mortgage : Mortgage.t;
    amortization : Amortization.t;
    results : Results.t;
  }

  let encode r =
    `Assoc
      [
        ("mortgage", Mortgage.encode r.mortgage);
        ("amortization", Amortization.encode r.amortization);
        ("results", Results.encode r.results);
      ]

  let decode json =
    let open Yojson.Basic.Util in
    try
      let mortgage = json |> member "mortgage" |> Mortgage.decode |> unwrap in
      let amortization =
        json |> member "amortization" |> Amortization.decode |> unwrap
      in
      let results = json |> member "results" |> Results.decode |> unwrap in
      Ok { mortgage; amortization; results }
    with exn -> Error (Printexc.to_string exn)
end

type mortgage_in_progress =
  | Mortgage_not_chosen
  | Mortgage_editing of EditableMortgage.t
  | Mortgage_chosen of Mortgage.t

type amortization_in_progress =
  | Amortization_editing of EditableAmortization.t
  | Amortization_chosen of Amortization.t

type report_in_progress = {
  mortgage : mortgage_in_progress;
  amortization : amortization_in_progress;
}
