let file_path = "data/day5.txt"

let input_file_to_split_strings file_path =
  (* read input into string *)
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let rec find_double_newline str id =
    if id < String.length str && str.[id] = '\n' && str.[id + 1] = '\n' then id
    else find_double_newline str (id + 1)
  in
  let split_index = find_double_newline content 0 in
  let rules =
    String.sub content 0 split_index
    |> String.split_on_char '\n'
    |> List.map (fun str ->
           match String.split_on_char '|' str with
           | [ a; b ] -> (int_of_string a, int_of_string b)
           | _ -> failwith "unexpected error formatting rules")
  in
  let updates =
    String.sub content (split_index + 2)
      (String.length content - (split_index + 3))
    |> String.split_on_char '\n'
    |> List.map (fun str ->
           String.split_on_char ',' str |> List.map int_of_string)
  in
  (rules, updates)

let new_status a b lst =
  match
    ( List.find_index (fun x -> x == a) lst,
      List.find_index (fun x -> x == b) lst )
  with
  | Some a, Some b -> a < b
  | _ -> true

let rec update_result update rules status =
  match rules with
  | [] when status -> List.nth update (List.length update / 2)
  | (a, b) :: rest when status ->
      update_result update rest (new_status a b update)
  | _ -> 0

let () =
  let rules, updates = input_file_to_split_strings file_path in
  (* part 1 *)
  let output =
    List.fold_left
      (fun acc update -> acc + update_result update rules true)
      0 updates
  in
  Printf.printf "\nsolution to part 1: %d\n" output
