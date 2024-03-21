open Units

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
