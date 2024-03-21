type t = {
  mortgage : Mortgage.t;
  amortization : Amortization.t;
  results : Report_results.t;
}

let encode r : Yojson.Basic.t =
  `Assoc
    [
      ("mortgage", Mortgage.encode r.mortgage);
      ("amortization", Amortization.encode r.amortization);
      ("results", Report_results.encode r.results);
    ]

let decode json =
  let open Yojson.Basic.Util in
  try
    let mortgage =
      json |> member "mortgage" |> Mortgage.decode |> Result_extra.unwrap
    in
    let amortization =
      json |> member "amortization" |> Amortization.decode
      |> Result_extra.unwrap
    in
    let results =
      json |> member "results" |> Report_results.decode |> Result_extra.unwrap
    in
    Ok { mortgage; amortization; results }
  with exn -> Error (Printexc.to_string exn)

let compare r1 r2 =
  match compare r1.mortgage.amount r2.mortgage.amount with
  | 0 -> (
      match compare r1.mortgage.years r2.mortgage.years with
      | 0 -> (
          match Amortization.compare r1.amortization r2.amortization with
          | 0 -> (
              match compare r1.mortgage.bank r2.mortgage.bank with
              | 0 -> 0
              | other -> other)
          | other -> other)
      | other -> other)
  | other -> other
