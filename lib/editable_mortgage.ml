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

let to_mortgage (m : t) : Mortgage.t =
  {
    bank = m.bank;
    rate =
      {
        first = float_of_string m.rate.first;
        rest = float_of_string m.rate.rest;
      };
    extra_expenses = float_of_string m.extra_expenses;
    amount = float_of_string m.amount;
    years = float_of_string m.years;
  }

let from_mortgage (m : Mortgage.t) : t =
  {
    bank = m.bank;
    rate =
      {
        first = string_of_float m.rate.first;
        rest = string_of_float m.rate.rest;
      };
    extra_expenses = string_of_float m.extra_expenses;
    amount = string_of_float m.amount;
    years = string_of_float m.years;
  }
