let file_path = "data/day4.test"

let traversal =
  [|
    [| -1; -1 |];
    [| -1; 0 |];
    [| -1; 1 |];
    [| 0; 1 |];
    [| 1; 1 |];
    [| 1; 0 |];
    [| 1; -1 |];
    [| 0; -1 |];
  |]

let search_word = "XMAS"

(** take input file and convert to 2d matrix with each element being a char *)
let input_file_to_matrix file_path =
  (* read input into string *)
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  let content = String.sub content 0 (String.length content - 1) in
  close_in ic;
  let matrix_1d = String.split_on_char '\n' content |> Array.of_list in
  (* understand this pls *)
  Array.map (fun row -> String.to_seq row |> Array.of_seq) matrix_1d

(*
let print_2d_array arr =
  Array.iter
    (fun row ->
      Array.iter
        (fun elem ->
          Printf.printf "%c " elem (* Print each element with a space *))
        row;
      Printf.printf "\n" (* New line after each row *))
    arr
*)

let word_exists_in_dir matrix pos dir =
  let n_rows = Array.length matrix in
  let n_cols = Array.length matrix.(0) in
  String.iteri
    (fun i letter ->
      let i_pos = pos.(0) + (dir.(0) * i) in
      let j_pos = pos.(1) + (dir.(1) * i) in
      if i_pos < 0 || i_pos >= n_rows || j_pos < 0 || j_pos >= n_cols then
        failwith "out of bounds"
      else
        match matrix.(i_pos).(j_pos) with
        | x when x = letter -> ()
        | _ -> failwith "word not found")
    search_word

let process_matrix matrix =
  let words = ref 0 in
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

(*
traverse through each item in matrix
if item is 'x' check all around it

top left -> -row, -col
top -> -row, 0
top right -> -row, +col
right -> 0, +col
bottom right -> +row, +col
bottom -> +row, 0
bottom left -> +row, -col
left -> 0, -col

multiplying by n will give nth item in direction, from current
see if chain of 4 give xmas
maybe somehow store in str and use index to multiply and check?
*)
