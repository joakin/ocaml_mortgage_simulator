open Units

type t = Yearly of currency

let encode (Yearly y) : Yojson.Basic.t =
  `Assoc [ ("kind", `String "Yearly"); ("value", `Float y) ]

let decode json =
  let open Yojson.Basic.Util in
  try
    let kind = json |> member "kind" |> to_string in
    match kind with
    | "Yearly" -> Ok (Yearly (json |> member "value" |> to_float))
    | _ -> Error (Printf.sprintf "Invalid kind: %s" kind)
  with exn -> Error (Printexc.to_string exn)

let compare (Yearly y1) (Yearly y2) = compare y1 y2
