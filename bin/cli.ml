let prompt ?default (label : string) =
  let default_str =
    match default with
    | Some "" | None -> ""
    | Some default -> Printf.sprintf " [%s]" default
  in
  Printf.printf "%s%s: " label default_str;
  let answer = read_line () in
  match (answer, default) with "", Some v -> v | v, _ -> v

let rec menu (items : (string * 'a) list) : 'a =
  List.iteri
    (fun i (label, _) -> print_endline (string_of_int (i + 1) ^ ". " ^ label))
    items;
  try
    print_string "> ";
    let choice = read_line () in
    let choice = int_of_string choice in
    let label, action = List.nth items (choice - 1) in
    Printf.printf "Selected \"%s\"\n\n" label;
    action
  with Failure _ ->
    print_endline "Invalid choice\n";
    menu items

let key_value_list (items : 'a list) ?(separator = "")
    (fields : (string * ('a -> string)) list) =
  print_endline separator;
  List.iteri
    (fun i item ->
      List.iteri
        (fun j (label, field) ->
          if j = 0 then Printf.printf "%i. %s: %s\n" i label (field item)
          else Printf.printf "   %s: %s\n" label (field item))
        fields;
      Printf.printf "%s\n\n" separator)
    items

let key_value (item : 'a) (fields : (string * ('a -> string)) list) =
  List.iter
    (fun (label, field) -> Printf.printf "%s: %s\n" label (field item))
    fields
