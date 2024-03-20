type years = float
type year = int
type month = int
type interest_rate = float
type currency = float

let unwrap = function Ok x -> x | Error e -> failwith e

module Mortgage : sig
  type rate = { first : interest_rate; rest : interest_rate }

  type t = {
    bank : string;
    rate : rate;
    extra_expenses : currency;
    amount : currency;
    years : years;
  }

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
  type t = Yearly of currency

  val encode : t -> Yojson.Basic.t
  val decode : Yojson.Basic.t -> (t, string) result
end = struct
  type t = Yearly of currency

  let encode (Yearly y) =
    `Assoc [ ("kind", `String "Yearly"); ("value", `Float y) ]

  let decode json =
    let open Yojson.Basic.Util in
    try
      let kind = json |> member "kind" |> to_string in
      match kind with
      | "Yearly" -> Ok (Yearly (json |> member "value" |> to_float))
      | _ -> Error (Printf.sprintf "Invalid kind: %s" kind)
    with exn -> Error (Printexc.to_string exn)
end

module EditableAmortization = struct
  type t = Yearly of string
end

module MonthlyRecord : sig
  type t = {
    year : year;
    month : month;
    monthly_payment : currency;
    interest_payed : currency;
    amortized : currency;
    extra_amortization : currency;
    remaining_loan : currency;
  }

  val encode : t -> Yojson.Basic.t
  val decode : Yojson.Basic.t -> (t, string) result
end = struct
  type t = {
    year : year;
    month : month;
    monthly_payment : currency;
    interest_payed : currency;
    amortized : currency;
    extra_amortization : currency;
    remaining_loan : currency;
  }

  let encode m =
    `Assoc
      [
        ("year", `Int m.year);
        ("month", `Int m.month);
        ("monthlyPayment", `Float m.monthly_payment);
        ("interestPayed", `Float m.interest_payed);
        ("amortized", `Float m.amortized);
        ("extraAmortization", `Float m.extra_amortization);
        ("remainingLoan", `Float m.remaining_loan);
      ]

  let decode json =
    let open Yojson.Basic.Util in
    try
      let year = json |> member "year" |> to_int in
      let month = json |> member "month" |> to_int in
      let monthly_payment = json |> member "monthlyPayment" |> to_float in
      let interest_payed = json |> member "interestPayed" |> to_float in
      let amortized = json |> member "amortized" |> to_float in
      let extra_amortization = json |> member "extraAmortization" |> to_float in
      let remaining_loan = json |> member "remainingLoan" |> to_float in
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
  type monthly_payment = { first : currency; rest : currency }

  type t = {
    monthly_payment : monthly_payment;
    finishes_paying_in : years;
    total_interests : currency;
    total_expenses_and_interests : currency;
    records : MonthlyRecord.t list;
  }

  let encode r =
    `Assoc
      [
        ( "monthly_payment",
          `Assoc
            [
              ("first", `Float r.monthly_payment.first);
              ("rest", `Float r.monthly_payment.rest);
            ] );
        ("finishes_paying_in", `Float r.finishes_paying_in);
        ("total_interests", `Float r.total_interests);
        ("total_expenses_and_interests", `Float r.total_expenses_and_interests);
        ("records", `List (List.map MonthlyRecord.encode r.records));
      ]

  let decode json =
    let open Yojson.Basic.Util in
    try
      let monthly_payment =
        json |> member "monthly_payment" |> fun json ->
        {
          first = json |> member "first" |> to_float;
          rest = json |> member "rest" |> to_float;
        }
      in
      let finishes_paying_in =
        json |> member "finishes_paying_in" |> to_float
      in
      let total_interests = json |> member "total_interests" |> to_float in
      let total_expenses_and_interests =
        json |> member "total_expenses_and_interests" |> to_float
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

(*
calculateMonthlyPayment : Currency -> InterestRate -> Years -> Currency
calculateMonthlyPayment amount rate years =
    (amount * (rate / 12)) / (100 * (1 - (1 + (rate / 12) / 100) ^ -(years * 12)))


isLoanPayed : Float -> Bool
isLoanPayed amount =
    (amount * 100 |> floor) == 0
*)

let calculate_monthly_payment (amount : currency) (rate : interest_rate)
    (years : years) : currency =
  let monthly_rate = rate /. 12. in
  let months = years *. 12. in
  let discount_factor = 1. -. ((1. +. (monthly_rate /. 100.)) ** -.months) in
  amount *. monthly_rate /. (100. *. discount_factor)

let is_loan_payed amount = amount |> Float.floor = 0.

let calculate_records (mortgage : Mortgage.t) (amortization : Amortization.t)
    (prev_record : MonthlyRecord.t) (until_month : month) : MonthlyRecord.t list
    =
  let rate month =
    if month <= 12 then mortgage.rate.first else mortgage.rate.rest
  in

  let calculate_month (last_record : MonthlyRecord.t) : MonthlyRecord.t =
    let month = last_record.month + 1 in
    let monthly_payment =
      if month = 13 && mortgage.rate.first <> mortgage.rate.rest then
        calculate_monthly_payment last_record.remaining_loan (rate month)
          (mortgage.years -. 1.)
      else last_record.monthly_payment
    in
    let interest_payed =
      last_record.remaining_loan *. (rate month /. 100. /. 12.)
    in
    let amortized =
      min (monthly_payment -. interest_payed) last_record.remaining_loan
    in
    let loan_amount_amortized = last_record.remaining_loan -. amortized in
    let extra_amortization =
      if is_loan_payed loan_amount_amortized then 0.
      else if month mod 12 = 0 then
        match amortization with
        | Yearly amount -> min amount loan_amount_amortized
      else 0.
    in
    let remaining_loan = max (loan_amount_amortized -. extra_amortization) 0. in
    {
      year = ((month - 1) / 12) + 1;
      month;
      monthly_payment;
      interest_payed;
      amortized;
      extra_amortization;
      remaining_loan;
    }
  in
  let rec iter (last_record : MonthlyRecord.t) (records : MonthlyRecord.t list)
      : MonthlyRecord.t list =
    if
      is_loan_payed last_record.remaining_loan
      || last_record.month > until_month
    then records
    else
      let record = calculate_month last_record in
      iter record (record :: records)
  in
  iter prev_record [] |> List.rev

let calculate_report_results (mortgage : Mortgage.t)
    (amortization : Amortization.t) : Results.t =
  let monthly_payment_first_year =
    calculate_monthly_payment mortgage.amount mortgage.rate.first mortgage.years
  in
  (* Base record to start generating all the monthly records. This is
     a bit hacky, given it relies in knowing that calculateRecords only
     uses the remainingLoan, month, and monthlyPayment from the previous
     record to operate.
  *)
  let base_record : MonthlyRecord.t =
    {
      year = 1;
      month = 0;
      monthly_payment = monthly_payment_first_year;
      interest_payed = 0.;
      amortized = 0.;
      extra_amortization = 0.;
      remaining_loan = mortgage.amount;
    }
  in
  let records =
    calculate_records mortgage amortization base_record
      (mortgage.years *. 12. |> Float.floor |> int_of_float)
  in
  let total_interests =
    List.fold_left
      (fun acc { MonthlyRecord.interest_payed; _ } -> acc +. interest_payed)
      0. records
  in
  let result : Results.t =
    {
      monthly_payment =
        {
          first = monthly_payment_first_year;
          rest = (try (List.nth records 13).monthly_payment with _ -> 0.);
        };
      finishes_paying_in = (List.length records |> float_of_int) /. 12.;
      total_interests;
      total_expenses_and_interests = total_interests +. mortgage.extra_expenses;
      records;
    }
  in
  result
