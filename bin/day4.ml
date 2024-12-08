let file_path = "data/day4.txt"

let traversal =
  [|
    (* top left *)
    [| -1; -1 |];
    (* top *)
    [| -1; 0 |];
    (* top right *)
    [| -1; 1 |];
    (* right *)
    [| 0; 1 |];
    (* bottom right *)
    [| 1; 1 |];
    (* bottom *)
    [| 1; 0 |];
    (* bottom left *)
    [| 1; -1 |];
    (* left *)
    [| 0; -1 |];
  |]

let search_word = "XMAS"

(** take input file and convert to 2d matrix with each element being a char *)
let input_file_to_matrix file_path =
  (* read input into string *)
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  (* ignore last char since it's newline and we're splitting on it *)
  let content = String.sub content 0 (String.length content - 1) in
  close_in ic;
  (* convert string to 2d array, splitting on newline and then converting string to sequence *)
  let matrix_1d = String.split_on_char '\n' content |> Array.of_list in
  Array.map (fun row -> String.to_seq row |> Array.of_seq) matrix_1d

(** check if search word exists from specific position, in traversal direction *)
let word_exists_in_dir matrix pos dir =
  let n_rows = Array.length matrix in
  let n_cols = Array.length matrix.(0) in
  String.iteri
    (fun i letter ->
      let i_pos = pos.(0) + (dir.(0) * i) in
      let j_pos = pos.(1) + (dir.(1) * i) in
      (* if not valid pos, fail *)
      if i_pos < 0 || i_pos >= n_rows || j_pos < 0 || j_pos >= n_cols then
        failwith "out of bounds"
      else
        match matrix.(i_pos).(j_pos) with
        | x when x = letter -> ()
        (* if letter doesn't match search word, fail *)
        | _ -> failwith "word not found")
    search_word

(** iterates through matrix and check if cell is 'X'.
    if found, check if "XMAS" exists in any traversal direction, and add to total. *)
let process_matrix matrix =
  let words = ref 0 in
  (* this is really messy but all it does is iterate through all elements
     if 'X' found, it traverses every direction to search for word *)
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j cell ->
          if cell = 'X' then
            (Array.iter (fun dir ->
                 try
                   word_exists_in_dir matrix [| i; j |] dir;
                   words := !words + 1
                 with Failure _ -> ()))
              traversal)
        row)
    matrix;
  !words

let () =
  let matrix = input_file_to_matrix file_path in

  (* part 1 *)
  Printf.printf "solution to part 1: %d\n" (process_matrix matrix)
