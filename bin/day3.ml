let file_path = "data/day3.txt"

(** take input file and convert to string *)
let input_file_to_string file_path =
  (* read input into string *)
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

(** takes an input string and returns bool of whether string is valid int *)
let is_valid_int x =
  try
    let _ = int_of_string x in
    true
  with Failure _ -> false

(** takes input string, meant to be instruction parameters, and checks if it's valid.
    returns some int tuple if valid. *)
let process_inputs str =
  let parts = String.split_on_char ',' str in
  match parts with
  | [ a; b ] when is_valid_int a && is_valid_int b ->
      Some (int_of_string a, int_of_string b)
  | _ -> None

(** given an m is found, takes position of m and returns an int.
   it's 0 if valid mul instructions not found. otherwise it's the product. *)
let process_mul str start_pos =
  (* check if valid mul instruction exists at m *)
  if start_pos + 4 < String.length str && String.sub str start_pos 4 = "mul("
  then
    try
      let end_pos = String.index_from str (start_pos + 4) ')' in
      (* if instructions found, try processing them and adding to total *)
      match
        process_inputs
          (String.sub str (start_pos + 4) (end_pos - start_pos - 4))
      with
      | Some (a, b) -> (a * b, end_pos)
      (* if parameters not valid, start searching after "mul(" *)
      | None -> (0, start_pos + 1)
      (* if closed bracket not found, no more valid instructions exist.
         return current result, with final index as no more checking needs to be done. *)
    with Not_found -> (0, String.length str)
    (* if 'm' found but "mul(" not found, just start searching from next pos *)
  else (0, start_pos + 1)

(** check if content starting with d is do() or don't(). if yes return new state. if not, return same state. *)
let process_d str start_pos state =
  if start_pos + 7 < String.length str && String.sub str start_pos 7 = "don't()"
  then (false, start_pos + 7)
  else if
    start_pos + 4 < String.length str && String.sub str start_pos 4 = "do()"
  then (true, start_pos + 4)
  else (state, start_pos + 1)

(** process input string to find instruction starts, process said instructions, and return final result *)
let process_instructions str conditionals =
  let rec process_string str pos res state =
    (* check if d or m found - start of valid instructions *)
    let next_m_found = String.index_from_opt str pos 'm' in
    let next_d_found = String.index_from_opt str pos 'd' in
    (* if starting found, match them based on what/where things are found *)
    match (next_m_found, next_d_found) with
    | Some m_pos, Some d_pos when d_pos < m_pos ->
        let new_state, next_pos = process_d str d_pos state in
        if conditionals then process_string str next_pos res new_state
        else process_string str next_pos res state
    | Some m_pos, _ ->
        let prod, next_pos = process_mul str m_pos in
        if state then process_string str next_pos (res + prod) state
        else process_string str next_pos res state
    (* if no valid instructions found, return current result *)
    | _ -> res
  in
  process_string str 0 0 true

let () =
  let content = input_file_to_string file_path in
  Printf.printf "solution to part 1: %d\n" (process_instructions content false);
  Printf.printf "solution to part 1: %d\n" (process_instructions content true)
