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

type page = Home | AddNewReport of report_in_progress | ViewReport of Report.t

type state = {
  mortgages : Mortgage.t list;
  reports : Report.t list;
  page : page;
}

type action =
  | NavigateToNewReport
  | NavigateToHome
  | SelectMortgage of Mortgage.t
  | NewMortgage
  | EditMortgage of Mortgage.t
  | UpdateEditingMortgage of Editable_mortgage.t
  | ResetMortgage
  | UpdateEditingAmortization of Editable_amortization.t
  | SelectAmortization of Amortization.t
  | SaveReport of Report_results.t
  | DownloadSession
  | LoadSession
  | SessionSelected of string (* this is a file? *)
  | SessionLoaded of string
  | OpenReportDetail of Report.t

let init () : state = { mortgages = []; reports = []; page = Home }

let update (action : action) (state : state) : state =
  match action with
  | NavigateToNewReport -> state
  | NavigateToHome -> state
  | SelectMortgage _ -> state
  | NewMortgage -> state
  | EditMortgage _ -> state
  | UpdateEditingMortgage _ -> state
  | ResetMortgage -> state
  | UpdateEditingAmortization _ -> state
  | SelectAmortization _ -> state
  | SaveReport _ -> state
  | DownloadSession -> state
  | LoadSession -> state
  | SessionSelected _ -> state
  | SessionLoaded _ -> state
  | OpenReportDetail _ -> state

let render (state : state) : action option =
  match state.page with
  | Home ->
      print_endline "Home";
      None
  | AddNewReport _ ->
      print_endline "Add new report";
      None
  | ViewReport _ ->
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
