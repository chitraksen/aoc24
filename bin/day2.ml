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

(** takes a report and measures if it's safe *)
let rec is_safe l =
  let increasing a b c = (c > b) && (b > a) && (c - b <= 3) && (b - a <= 3) in
  let decreasing a b c = (c < b) && (b < a) && (b - c <= 3) && (a - b <= 3) in
  match l with
  | a :: b :: c :: rest -> (increasing a b c || decreasing a b c) && is_safe (b :: c :: rest)
  | a :: b :: _ -> 1 <= abs (a - b) && abs (a - b) <= 3
  | _ -> true

(** takes a list of reports as input and returns how many of them are safe *)
let rec number_of_safe_reports data =
  match data with
  | [] -> 0
  | report :: rest -> (if is_safe report then 1 else 0) + number_of_safe_reports rest

let () =
  let data = input_file_to_2d_list file_path in

  (* part 1 *)
  let safe_reports = number_of_safe_reports data in
  print_string "solution to part 1: "; print_int safe_reports; print_endline ""
