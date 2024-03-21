open Units

type t = {
  year : year;
  month : month;
  monthly_payment : currency;
  interest_payed : currency;
  amortized : currency;
  extra_amortization : currency;
  remaining_loan : currency;
}

let encode m : Yojson.Basic.t =
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

let calculate (mortgage : Mortgage.t) (amortization : Amortization.t)
    (last_record : t) : t =
  let rate month =
    if month <= 12 then mortgage.rate.first else mortgage.rate.rest
  in
  let month = last_record.month + 1 in
  let monthly_payment =
    if month = 13 && mortgage.rate.first <> mortgage.rate.rest then
      Util.calculate_monthly_payment last_record.remaining_loan (rate month)
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
    if Util.is_loan_payed loan_amount_amortized then 0.
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

let calculate_month_records (mortgage : Mortgage.t)
    (amortization : Amortization.t) (prev_record : t) (until_month : month) :
    t list =
  let rec iter (last_record : t) (records : t list) : t list =
    if
      Util.is_loan_payed last_record.remaining_loan
      || last_record.month > until_month
    then records
    else
      let record = calculate mortgage amortization last_record in
      iter record (record :: records)
  in
  iter prev_record [] |> List.rev
