let file_path = "data/day7.txt"

(** input file to format *)
let input_file_to_format file_path =
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  String.split_on_char '\n' content
  |> List.filter (fun x -> String.length x > 1)

(** convert int to base [base] number as int list of fixed [length] *)
let to_baseX_fix_len n base length =
  let rec aux n i acc =
    if i = 0 then acc else aux (n / base) (i - 1) ((n mod base) :: acc)
  in
  aux n length []

(* 0 is add, 1 is mul, 2 is concat *)

(** take input the equation and number of operators.
    return result if solution exists *)
let check_equation input num_ops =
  let div_pos = String.index input ':' in
  let res = String.sub input 0 div_pos |> int_of_string in
  let arg_list =
    String.sub input (div_pos + 2) (String.length input - div_pos - 2)
    |> String.split_on_char ' '
    |> List.map (fun x -> int_of_string x)
  in
  let num_args = List.length arg_list in
  let concat_ints a b = int_of_string (string_of_int a ^ string_of_int b) in
  let rec check_equality op_seq =
    (* get current operation permutation, using [op_seq] as seed.
       done by converting number to base X for X operators*)
    let op_list = to_baseX_fix_len op_seq num_ops num_args in
    let rec calc_args i acc =
      if i = num_args then acc
      else if acc < 0 then 0 (* max int stopping case *)
      else if acc > res then 0 (* early stopping *)
      else
        match List.nth op_list i with
        | 0 -> calc_args (i + 1) (acc + List.nth arg_list i)
        | 1 -> calc_args (i + 1) (acc * List.nth arg_list i)
        | 2 -> calc_args (i + 1) (concat_ints acc (List.nth arg_list i))
        | _ -> failwith "unexpected error calculating result"
    in
    if calc_args 0 0 = res then res
    else if
      (* last sequence reached, but calc_args not eq *)
      op_seq + 1
      = int_of_float (float_of_int num_ops ** float_of_int (num_args - 1))
    then 0
    else check_equality (op_seq + 1)
  in
  check_equality 0

let () =
  let input = input_file_to_format file_path in
  let temp = List.fold_left (fun acc x -> acc + check_equation x 2) 0 input in
  Printf.printf "solution to part 1: %d\n" temp;
  let temp = List.fold_left (fun acc x -> acc + check_equation x 3) 0 input in
  Printf.printf "solution to part 2: %d\n" temp
