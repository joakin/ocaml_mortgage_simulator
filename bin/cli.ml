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
      List.iter
        (fun (label, field) ->
          Printf.printf "%i. %s: %s\n" i label (field item))
        fields;
      print_endline separator)
    items
