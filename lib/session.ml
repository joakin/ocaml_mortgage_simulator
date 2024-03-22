type t = { mortgages : Mortgage.t list; reports : Report.t list }

let encode (s : t) : Yojson.Basic.t =
  `Assoc
    [
      ("mortgages", `List (List.map Mortgage.encode s.mortgages));
      ("reports", `List (List.map Report.encode s.reports));
    ]

let decode json =
  let open Yojson.Basic.Util in
  try
    let mortgages =
      json |> member "mortgages" |> to_list
      |> List.map (fun json -> Mortgage.decode json |> Result_extra.unwrap)
    in
    let reports =
      json |> member "reports" |> to_list
      |> List.map (fun json -> Report.decode json |> Result_extra.unwrap)
    in
    Ok { mortgages; reports }
  with exn -> Error (Printexc.to_string exn)

let download_file (session : t) (filename : string) : unit =
  let json = encode session in
  Yojson.Basic.pretty_to_channel (open_out filename) json

let load_file (filename : string) : (t, string) result =
  let json = Yojson.Basic.from_file filename in
  decode json
