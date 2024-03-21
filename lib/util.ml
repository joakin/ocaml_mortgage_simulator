open Units

let calculate_monthly_payment (amount : currency) (rate : interest_rate)
    (years : years) : currency =
  let monthly_rate = rate /. 12. in
  let months = years *. 12. in
  let discount_factor = 1. -. ((1. +. (monthly_rate /. 100.)) ** -.months) in
  amount *. monthly_rate /. (100. *. discount_factor)

let is_loan_payed amount = amount |> Float.floor = 0.
