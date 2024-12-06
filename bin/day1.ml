let file_path = "data/day1.txt"

(** convert input from txt file to 2 sorted lists *)
let input_file_to_sorted_list file_path =
  (* open input channel *)
  let input_channel = open_in file_path in
  (* recursively read lines, convert to int, and add to list *)
  let rec process_lines l1 l2 =
    try
      let line = input_line input_channel in
      (* split string on space, and exclude empty items extracted *)
      match String.split_on_char ' ' line |> List.filter (fun s -> s <> "") with
      (* check for exact 2 ints match *)
      | [ x; y ] ->
          let int_x = int_of_string x in
          let int_y = int_of_string y in
          process_lines (int_x :: l1) (int_y :: l2)
      | _ -> raise (Failure "error processing input")
    with End_of_file ->
      close_in input_channel;
      (* on reach end of file, return 2 sorted lists *)
      (List.sort compare l1, List.sort compare l2)
  in
  process_lines [] []

(** find total distance given 2 sorted lists *)
let rec find_total_distance l1 l2 =
  match (l1, l2) with
  | [], [] -> 0
  | a :: rest1, b :: rest2 -> abs (a - b) + find_total_distance rest1 rest2
  | _ -> raise (Failure "error finding distance")

(** find frequency of elements in list, given a sorted list *)
let rec find_frequency l n =
  match l with
  | x :: rest when x < n -> find_frequency rest n
  | x :: rest when x == n -> 1 + find_frequency rest n
  | _ -> 0

(** find similarity score given a sorted l2 *)
let rec find_similarity_score l1 l2 =
  match l1 with
  | [] -> 0
  | x :: rest -> (x * find_frequency l2 x) + find_similarity_score rest l2

(* entry point *)
let () =
  (* get the 2 lists - after sorting them *)
  let l1, l2 = input_file_to_sorted_list file_path in

  (* find answer to part 1 *)
  let total_distance = find_total_distance l1 l2 in
  print_string "solution to part 1: ";
  print_int total_distance;
  print_endline "";

  (* find answer to part 2 *)
  let similarity_score = find_similarity_score l1 l2 in
  print_string "solution to part 2: ";
  print_int similarity_score;
  print_endline ""
