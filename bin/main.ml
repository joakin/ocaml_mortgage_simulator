open Mortgage_simulator

type mortgage_in_progress =
  | Mortgage_not_chosen
  | Mortgage_editing of Editable_mortgage.t
  | Mortgage_chosen of Mortgage.t

type amortization_in_progress =
  | Amortization_editing of Editable_amortization.t
  | Amortization_chosen of Amortization.t

type report_in_progress = {
  mortgage : mortgage_in_progress;
  amortization : amortization_in_progress;
}

type screen =
  | Home
  | Add_new_report of report_in_progress
  | View_report of Report.t

type state = { session : Session.t; screen : screen }

type action =
  | Navigate_to_new_report
  | Navigate_to_home
  | Select_mortgage of Mortgage.t
  | New_mortgage
  | Edit_mortgage of Mortgage.t
  | Update_editing_mortgage of Editable_mortgage.t
  | Reset_mortgage
  | Update_editing_amortization of Editable_amortization.t
  | Select_amortization of Amortization.t
  | Save_report of Report_results.t
  | Download_session
  | Load_session
  | Open_report_detail of Report.t

let init () : state =
  { session = { mortgages = []; reports = [] }; screen = Home }

let update (action : action) (state : state) : state =
  match action with
  | Navigate_to_new_report ->
      {
        state with
        screen =
          Add_new_report
            {
              mortgage = Mortgage_not_chosen;
              amortization = Amortization_chosen (Amortization.Yearly 0.0);
            };
      }
  | Navigate_to_home -> { state with screen = Home }
  | Select_mortgage mortgage -> (
      match state.screen with
      | Add_new_report in_progress_report ->
          {
            state with
            screen =
              Add_new_report
                { in_progress_report with mortgage = Mortgage_chosen mortgage };
          }
      | _ -> state)
  | New_mortgage -> (
      match state.screen with
      | Add_new_report in_progress_report ->
          {
            state with
            screen =
              Add_new_report
                {
                  in_progress_report with
                  mortgage = Mortgage_editing Editable_mortgage.empty;
                };
          }
      | _ -> state)
  | Edit_mortgage mortgage -> (
      match state.screen with
      | Add_new_report in_progress_report ->
          {
            state with
            screen =
              Add_new_report
                {
                  in_progress_report with
                  mortgage =
                    Mortgage_editing (Editable_mortgage.from_mortgage mortgage);
                };
          }
      | _ -> state)
  | Update_editing_mortgage editable_mortgage -> (
      match state.screen with
      | Add_new_report in_progress_report ->
          {
            state with
            screen =
              Add_new_report
                {
                  in_progress_report with
                  mortgage = Mortgage_editing editable_mortgage;
                };
          }
      | _ -> state)
  | Reset_mortgage -> (
      match state.screen with
      | Add_new_report in_progress_report ->
          {
            state with
            screen =
              Add_new_report
                { in_progress_report with mortgage = Mortgage_not_chosen };
          }
      | _ -> state)
  | Update_editing_amortization amortization -> (
      match state.screen with
      | Add_new_report in_progress_report ->
          {
            state with
            screen =
              Add_new_report
                {
                  in_progress_report with
                  amortization = Amortization_editing amortization;
                };
          }
      | _ -> state)
  | Select_amortization amortization -> (
      match state.screen with
      | Add_new_report in_progress_report ->
          {
            state with
            screen =
              Add_new_report
                {
                  in_progress_report with
                  amortization = Amortization_chosen amortization;
                };
          }
      | _ -> state)
  | Save_report report_results -> (
      match state.screen with
      | Add_new_report in_progress_report -> (
          match
            (in_progress_report.mortgage, in_progress_report.amortization)
          with
          | Mortgage_chosen mortgage, Amortization_chosen amortization ->
              let report : Report.t =
                { mortgage; amortization; results = report_results }
              in
              {
                screen = Home;
                session =
                  {
                    reports =
                      (* Don't add reports if there is another one like it.
                         Don't compare report results since they are derived from
                         the other two parameters. *)
                      (if
                         List.exists
                           (fun (report : Report.t) ->
                             amortization == report.amortization
                             && mortgage == report.mortgage)
                           state.session.reports
                       then state.session.reports
                       else
                         report :: state.session.reports
                         |> List.sort Report.compare);
                    mortgages =
                      (* Don't add mortgage if there is another one like it *)
                      (if List.exists (( = ) mortgage) state.session.mortgages
                       then state.session.mortgages
                       else mortgage :: state.session.mortgages);
                  };
              }
          | _ -> state)
      | _ -> state)
  | Download_session ->
      Session.download_file state.session "session.json";
      print_endline "Session saved";
      state
  | Load_session -> (
      let session = Session.load_file "session.json" in
      match session with
      | Ok session -> { state with session }
      | Error e ->
          print_endline e;
          state)
  | Open_report_detail report -> { state with screen = View_report report }

let main_menu =
  [
    (I18n.labels.home, Navigate_to_home);
    (I18n.labels.new_report, Navigate_to_new_report);
    (I18n.labels.download_session, Download_session);
    (I18n.labels.load_session, Load_session);
  ]

let rec menu (items : (string * 'a) list) : 'a =
  List.iteri
    (fun i (label, _) -> print_endline (string_of_int (i + 1) ^ ". " ^ label))
    items;
  try
    print_string "> ";
    let choice = read_line () in
    let choice = int_of_string choice in
    let label, action = List.nth items (choice - 1) in
    Printf.printf "Selected \"%s\"\n" label;
    action
  with Failure _ ->
    print_endline "Invalid choice";
    menu items

let render (state : state) : action option =
  match state.screen with
  | Home ->
      print_endline "Home";
      Some (menu main_menu)
  | Add_new_report _ ->
      print_endline "Add new report";
      None
  | View_report _ ->
      print_endline "View report";
      None

let run () =
  let state = init () in
  let rec loop (state : state) =
    let action = render state in
    match action with Some action -> loop (update action state) | None -> ()
  in
  loop state
;;

run ()
