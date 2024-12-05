let file_path = "data/day2.txt"

(** take input file and convert it into list of lists.
    every element in the main list is a singular report.
*)
let input_file_to_2d_list file_path = 
  let ic = open_in file_path in
  let rec process_lines l =
    try
      let line = input_line ic in
      let report = List.map int_of_string (String.split_on_char ' ' line) in
      process_lines (report :: l)
    with End_of_file ->
      close_in ic;
      l
  in process_lines []

(* check if difference between two elements is between 1 and 3 *)
let is_valid_diff a b = 
  let diff = abs (a - b) in
  diff >= 1 && diff <= 3

(* check if a list is monotonically increasing or decreasing *)
let is_valid_report lst =
  let rec check_monotonic trend = function
    | [] | [_] -> true
    | a :: b :: rest ->
        match trend with
        | None -> 
            (* initial trend *)
            if is_valid_diff a b then
              let new_trend = if a < b then Some true else Some false
              in check_monotonic new_trend (b :: rest)
            else false
        | Some is_increasing ->
            if not (is_valid_diff a b) then false
            else
              (* check trend is maintained *)
              let current_comparison = 
                if is_increasing then a < b else a > b 
              in
              if not current_comparison then false
              else check_monotonic trend (b :: rest)
  in
  check_monotonic None lst

(** given a report and dampener availability, check if report is safe *)
let check_report_safety lst dampening =
  (* check if the report is already valid *)
  if is_valid_report lst then true
  else if dampening then
    (* if dampener available remove each element and check if the resulting report is valid *)
    let check_removal lst =
      let rec check_report_without index =
        if index >= List.length lst then false
        else 
          let removed = List.filteri (fun i _ -> i <> index) lst in
          if is_valid_report removed then true
          else check_report_without (index + 1)
      in
      check_report_without 0
    in
    check_removal lst
  else false

(** takes a list of reports as input and returns how many of them are safe *)
let rec number_of_safe_reports data dampening =
  match data with
  | [] -> 0
  | report :: rest -> (if check_report_safety report dampening then 1 else 0) + number_of_safe_reports rest dampening

let () =
  let data = input_file_to_2d_list file_path in

  (* part 1 *)
  let safe_reports = number_of_safe_reports data false in
  print_string "solution to part 1: "; print_int safe_reports; print_endline "";

  (* part 2 *)
  let safe_reports_dampened = number_of_safe_reports data true in
  print_string "solution to part 2: "; print_int safe_reports_dampened; print_endline ""
