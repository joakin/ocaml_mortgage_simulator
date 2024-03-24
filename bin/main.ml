open Mortgage_simulator

type report_in_progress =
  | Mortgage_not_chosen
  | Mortgage_editing of Editable_mortgage.t
  | Mortgage_chosen of Mortgage.t
  | Amortization_editing of (Mortgage.t * Editable_amortization.t)
  | Amortization_chosen of (Mortgage.t * Amortization.t)

type screen =
  | Home
  | Add_new_report of report_in_progress
  | View_report of Report.t

type state = { session : Session.t; screen : screen }

type action =
  | Navigate_to_home
  | Navigate_to_new_report
  | New_mortgage
  | Edit_mortgage of Mortgage.t
  | Select_mortgage of Mortgage.t
  | Reset_mortgage
  | New_amortization
  | Select_amortization of Amortization.t
  | Save_report of Report_results.t
  | Download_session
  | Load_session
  | Open_report_detail of Report.t

let init () : state =
  { session = { mortgages = []; reports = [] }; screen = Home }

let update (action : action) (state : state) : state =
  match action with
  | Navigate_to_home -> { state with screen = Home }
  | Navigate_to_new_report ->
      { state with screen = Add_new_report Mortgage_not_chosen }
  | New_mortgage -> (
      match state.screen with
      | Add_new_report Mortgage_not_chosen ->
          {
            state with
            screen = Add_new_report (Mortgage_editing Editable_mortgage.empty);
          }
      | _ -> state)
  | Edit_mortgage mortgage -> (
      match state.screen with
      | Add_new_report Mortgage_not_chosen | Add_new_report (Mortgage_chosen _)
        ->
          {
            state with
            screen =
              Add_new_report
                (Mortgage_editing (Editable_mortgage.from_mortgage mortgage));
          }
      | _ -> state)
  | Select_mortgage mortgage -> (
      match state.screen with
      | Add_new_report Mortgage_not_chosen | Add_new_report (Mortgage_editing _)
        ->
          { state with screen = Add_new_report (Mortgage_chosen mortgage) }
      | _ -> state)
  | Reset_mortgage -> (
      match state.screen with
      | Add_new_report _ ->
          { state with screen = Add_new_report Mortgage_not_chosen }
      | _ -> state)
  | New_amortization -> (
      match state.screen with
      | Add_new_report (Mortgage_chosen mortgage)
      | Add_new_report (Amortization_editing (mortgage, _))
      | Add_new_report (Amortization_chosen (mortgage, _)) ->
          {
            state with
            screen =
              Add_new_report
                (Amortization_editing
                   (mortgage, Editable_amortization.Yearly "0"));
          }
      | _ -> state)
  | Select_amortization amortization -> (
      match state.screen with
      | Add_new_report (Amortization_editing (mortgage, _)) ->
          {
            state with
            screen =
              Add_new_report (Amortization_chosen (mortgage, amortization));
          }
      | _ -> state)
  | Save_report report_results -> (
      match state.screen with
      | Add_new_report (Amortization_chosen (mortgage, amortization)) ->
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
                     report :: state.session.reports |> List.sort Report.compare);
                mortgages =
                  (* Don't add mortgage if there is another one like it *)
                  (if List.exists (( = ) mortgage) state.session.mortgages then
                     state.session.mortgages
                   else mortgage :: state.session.mortgages);
              };
          }
      | _ -> state)
  | Download_session ->
      Session.download_file state.session "session.json";
      print_endline "Session saved";
      state
  | Load_session -> (
      let session = Session.load_file "session.json" in
      match session with
      | Ok session ->
          print_endline "Session loaded";
          { state with session }
      | Error e ->
          print_endline e;
          state)
  | Open_report_detail report -> { state with screen = View_report report }

let heading_separator ?(sep = '=') title =
  print_endline (String.make (String.length title) sep)

let heading ?sep title =
  print_endline title;
  heading_separator ?sep title;
  print_endline ""

let main_menu =
  [
    (I18n.labels.home, Navigate_to_home);
    (I18n.labels.new_report, Navigate_to_new_report);
    (I18n.labels.download_session, Download_session);
    (I18n.labels.load_session, Load_session);
  ]

let view_mortgage (mortgage : Mortgage.t) =
  Cli.key_value mortgage
    [
      (I18n.labels.bank, fun (m : Mortgage.t) -> m.bank);
      ( I18n.labels.loan_amount,
        fun (m : Mortgage.t) -> I18n.labels.format_currency m.amount );
      ( I18n.labels.years,
        fun (m : Mortgage.t) -> I18n.labels.format_number m.years );
      ( I18n.labels.rate_first_year,
        fun (m : Mortgage.t) -> I18n.labels.format_number m.rate.first );
      ( I18n.labels.rate_rest_of_years,
        fun (m : Mortgage.t) -> I18n.labels.format_number m.rate.rest );
      ( I18n.labels.extra_expenses,
        fun (m : Mortgage.t) -> I18n.labels.format_currency m.extra_expenses );
    ]

let view_reports (reports : Report.t list) =
  if reports = [] then Printf.printf "%s\n\n" I18n.labels.no_reports_saved
  else
    Cli.key_value_list reports ~separator:"---------------------------------"
      [
        (I18n.labels.bank, fun (report : Report.t) -> report.mortgage.bank);
        ( I18n.labels.amount,
          fun (report : Report.t) ->
            I18n.labels.format_currency report.mortgage.amount );
        ( I18n.labels.years,
          fun (report : Report.t) ->
            I18n.labels.format_number report.mortgage.years );
        ( I18n.labels.rate_first_and_rest,
          fun (report : Report.t) ->
            I18n.labels.format_number report.mortgage.rate.first
            ^ " / "
            ^ I18n.labels.format_number report.mortgage.rate.rest );
        ( I18n.labels.extra_expenses,
          fun (report : Report.t) ->
            I18n.labels.format_currency report.mortgage.extra_expenses );
        ( I18n.labels.amortize_every_year,
          fun (report : Report.t) -> Amortization.format report.amortization );
        ( I18n.labels.total_expenses_and_interests,
          fun (report : Report.t) ->
            I18n.labels.format_currency
              report.results.total_expenses_and_interests );
        ( I18n.labels.payed_in,
          fun (report : Report.t) ->
            I18n.labels.format_years report.results.finishes_paying_in );
        ( I18n.labels.monthly_payment_first_year,
          fun (report : Report.t) ->
            I18n.labels.format_currency report.results.monthly_payment.first );
        ( I18n.labels.monthly_payment_rest,
          fun (report : Report.t) ->
            I18n.labels.format_currency report.results.monthly_payment.rest );
      ]

let view_in_progress_report (saved_mortgages : Mortgage.t list)
    (in_progress_report : report_in_progress) : action =
  let sep = '-' in
  match in_progress_report with
  | Mortgage_not_chosen -> (
      heading ~sep I18n.labels.select_a_mortgage;
      let chosen_mortgage =
        Cli.menu
          ((saved_mortgages
           |> List.map (fun (m : Mortgage.t) ->
                  ( Printf.sprintf "%s  %s:%s  %s:%s  %s:%s  %s:%s  %s:%s"
                      m.bank I18n.labels.loan_amount
                      (I18n.labels.format_currency m.amount)
                      I18n.labels.years
                      (I18n.labels.format_number m.years)
                      I18n.labels.rate_first_year
                      (I18n.labels.format_number m.rate.first)
                      I18n.labels.rate_rest_of_years
                      (I18n.labels.format_number m.rate.rest)
                      I18n.labels.extra_expenses
                      (I18n.labels.format_currency m.extra_expenses),
                    Some m )))
          @ [ (I18n.labels.new_mortgage, None) ])
      in
      match chosen_mortgage with
      | None -> New_mortgage
      | Some chosen_mortgage -> Select_mortgage chosen_mortgage)
  | Mortgage_editing editable_mortgage ->
      heading ~sep I18n.labels.edit_current_mortgage;
      let mortgage : Mortgage.t =
        Editable_mortgage.to_mortgage
          {
            bank =
              Cli.prompt I18n.labels.bank_name ~default:editable_mortgage.bank;
            rate =
              {
                first =
                  Cli.prompt I18n.labels.rate_first_year
                    ~default:editable_mortgage.rate.first;
                rest =
                  Cli.prompt I18n.labels.rate_rest_of_years
                    ~default:editable_mortgage.rate.rest;
              };
            extra_expenses =
              Cli.prompt I18n.labels.extra_expenses
                ~default:editable_mortgage.extra_expenses;
            amount =
              Cli.prompt I18n.labels.loan_amount
                ~default:editable_mortgage.amount;
            years =
              Cli.prompt I18n.labels.years ~default:editable_mortgage.years;
          }
      in
      Select_mortgage mortgage
  | Mortgage_chosen mortgage ->
      view_mortgage mortgage;
      print_endline "";
      Cli.menu
        [
          (I18n.labels.edit_current_mortgage, Edit_mortgage mortgage);
          (I18n.labels.change_mortgage, Reset_mortgage);
          (I18n.labels.continue, New_amortization);
        ]
  | Amortization_editing (mortgage, Editable_amortization.Yearly a) -> (
      heading ~sep I18n.labels.select_amortization;
      let amortization =
        Cli.prompt I18n.labels.amortize_every_year ~default:a
        |> fun amortization -> Editable_amortization.Yearly amortization
      in
      match Editable_amortization.to_amortization amortization with
      | Some amortization -> Select_amortization amortization
      | None ->
          print_endline "Invalid amortization";
          New_amortization)
  | Amortization_chosen (mortgage, (Amortization.Yearly a as amortization)) ->
      view_mortgage mortgage;
      print_endline "";
      Cli.key_value a
        [ (I18n.labels.amortize_every_year, I18n.labels.format_currency) ];
      print_endline "";
      let report_results =
        Report_results.calculate_report_results mortgage amortization
      in
      (* TODO: View report results *)
      Cli.menu
        [
          (I18n.labels.edit_amortization, New_amortization);
          (I18n.labels.change_mortgage, Edit_mortgage mortgage);
          (I18n.labels.save_report, Save_report report_results);
        ]

let render (state : state) : action option =
  (* clear screen *)
  print_string "\027[2J";
  (* cursor to home position *)
  print_string "\027[H";
  match state.screen with
  | Home ->
      heading I18n.labels.home;
      view_reports state.session.reports;
      Some (Cli.menu main_menu)
  | Add_new_report in_progress_report ->
      heading I18n.labels.new_report;
      Some (view_in_progress_report state.session.mortgages in_progress_report)
  | View_report _ ->
      heading I18n.labels.report_results;
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
