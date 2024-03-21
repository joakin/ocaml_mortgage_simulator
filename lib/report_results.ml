open Units

type monthly_payment = { first : currency; rest : currency }

type t = {
  monthly_payment : monthly_payment;
  finishes_paying_in : years;
  total_interests : currency;
  total_expenses_and_interests : currency;
  records : Monthly_record.t list;
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
      ("records", `List (List.map Monthly_record.encode r.records));
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
    let finishes_paying_in = json |> member "finishes_paying_in" |> to_float in
    let total_interests = json |> member "total_interests" |> to_float in
    let total_expenses_and_interests =
      json |> member "total_expenses_and_interests" |> to_float
    in
    let records =
      json |> member "records" |> to_list
      |> List.map (fun json ->
             json |> Monthly_record.decode |> Result_extra.unwrap)
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

let calculate_report_results (mortgage : Mortgage.t)
    (amortization : Amortization.t) : t =
  let monthly_payment_first_year =
    Util.calculate_monthly_payment mortgage.amount mortgage.rate.first
      mortgage.years
  in
  (* Base record to start generating all the monthly records. This is
     a bit hacky, given it relies in knowing that calculateRecords only
     uses the remainingLoan, month, and monthlyPayment from the previous
     record to operate.
  *)
  let base_record : Monthly_record.t =
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
    Monthly_record.calculate_month_records mortgage amortization base_record
      (mortgage.years *. 12. |> Float.floor |> int_of_float)
  in
  let total_interests =
    List.fold_left
      (fun acc { Monthly_record.interest_payed; _ } -> acc +. interest_payed)
      0. records
  in
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
