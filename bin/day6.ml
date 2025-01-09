let file_path = "data/day6.test"

(** input file to 2d array that is used as map *)
let input_file_to_array file_path =
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let output =
    String.split_on_char '\n' content
    |> List.filter (fun x -> String.length x > 1)
    |> Array.of_list
    |> Array.map (fun x -> String.to_seq x |> Array.of_seq)
  in
  output

(*
let print_map map =
  print_newline ();
  Array.iter
    (fun row ->
      Array.iter (fun c -> Printf.printf "%c " c) row;
      print_newline ())
    map
  *)

(** position of guard - (row, col) 0 indexed - if not found col is always -1 *)
let find_guard_pos map =
  let row =
    Array.find_index
      (Array.exists (fun x -> List.mem x [ '^'; '>'; 'v'; '<' ]))
      map
    |> Option.value ~default:(-1)
  in
  if row == -1 then (-1, -1)
  else
    let col =
      Array.find_index
        (fun x -> List.mem x [ '^'; '>'; 'v'; '<' ])
        (Array.get map row)
      |> Option.value ~default:(-1)
    in
    (row, col)

(** return next map state from map when guard is at row, col *)
let update_map (map : char array array) row col =
  let update_table =
    Hashtbl.of_seq
      (List.to_seq
         [
           ('^', (-1, 0, '>'));
           ('>', (0, 1, 'v'));
           ('v', (1, 0, '<'));
           ('<', (0, -1, '^'));
         ])
  in
  let row_inc, col_inc, new_dir = Hashtbl.find update_table map.(row).(col) in
  let new_row = row + row_inc in
  let new_col = col + col_inc in
  try
    if map.(new_row).(new_col) == '#' then map.(row).(col) <- new_dir
    else (
      map.(new_row).(new_col) <- map.(row).(col);
      map.(row).(col) <- 'X');
    map
  with Invalid_argument _ ->
    map.(row).(col) <- 'X';
    map

(** number of traversed positions in map k *)
let count_traversed map =
  let traversed_in_row =
    Array.map
      (fun x ->
        Array.fold_left (fun acc y -> if y == 'X' then acc + 1 else acc) 0 x)
      map
  in
  Array.fold_left (fun acc x -> acc + x) 0 traversed_in_row

(** simulate the next map state - assumes guard is present in map passed *)
let rec simulate_movement map =
  match find_guard_pos map with
  | _, b when b == -1 -> map
  | a, b -> simulate_movement (update_map map a b)

let () =
  (* part 1 *)
  let output_p1 =
    input_file_to_array file_path |> simulate_movement |> count_traversed
  in
  Printf.printf "\n%d\n" output_p1
