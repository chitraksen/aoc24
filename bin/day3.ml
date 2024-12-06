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

(** process input string to find instructions, process said instructions, and return final result *)
let process_instructions str =
  let rec process_string str pos res =
    try
      let start_pos = String.index_from str pos 'm' in
      (* if 'm' found, check existence of "mul(" *)
      if
        start_pos + 4 < String.length str && String.sub str start_pos 4 = "mul("
      then
        try
          let end_pos = String.index_from str (start_pos + 4) ')' in
          (* if instructions found, try processing them and adding to total *)
          match
            process_inputs
              (String.sub str (start_pos + 4) (end_pos - start_pos - 4))
          with
          | Some (a, b) ->
              let prod = a * b in
              process_string str (end_pos + 1) (res + prod)
          (* if parameters not valid, start searching after "mul(" *)
          | None -> process_string str (start_pos + 4) res
          (* if closed bracket not found, no more valid instructions exist -> return current result. *)
        with Not_found -> res
        (* if 'm' found but "mul(" not found, just start searching from next pos *)
      else process_string str (start_pos + 1) res
      (* if 'm' not found, no more valid instructions exist -> return current result *)
    with Not_found -> res
  in
  process_string str 0 0

let () =
  let res = file_path |> input_file_to_string |> process_instructions in
  Printf.printf "solution to part 1: %d\n" res
