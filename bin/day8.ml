let file_path = "data/day8.txt"

type position = int * int
(** position in grid of format row, col *)

(** input file to list of strings that's used as grid *)
let input_file_to_format file_path =
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  String.split_on_char '\n' content
  |> List.filter (fun x -> String.length x > 1)

(** takes input and returns position, and names of all antennas *)
let get_antennas input =
  let antennas : (position, char) Hashtbl.t = Hashtbl.create 5 in
  List.iteri
    (fun i row ->
      String.iteri
        (fun j letter ->
          if letter <> '.' then Hashtbl.add antennas (i, j) letter)
        row)
    input;
  antennas

(** solution to part 1 *)
let part1 input =
  let antennas = get_antennas input in
  let antinodes : (position, unit) Hashtbl.t = Hashtbl.create 10 in
  (* check if [x, y] is an antinode *)
  let is_antinode x1 y1 =
    let found = ref false in
    Hashtbl.iter
      (fun (x2, y2) antenna ->
        (* if not already found, and skip if looking at own pos *)
        if (not !found) && (x1, y1) <> (x2, y2) then
          (* new position, twice length in same line *)
          let new_pos = ((2 * x2) - x1, (2 * y2) - y1) in
          (* if same antenna found in new pos, mutate found to true *)
          match Hashtbl.find_opt antennas new_pos with
          | Some found_antenna -> if antenna = found_antenna then found := true
          | None -> ())
      antennas;
    !found
  in
  (* iterate through all positions and check if antinode *)
  List.iteri
    (fun i row ->
      String.iteri
        (fun j _ -> if is_antinode i j then Hashtbl.replace antinodes (i, j) ())
        row)
    input;
  (* count number of antinodes *)
  Hashtbl.length antinodes

(** solution to part 2 *)
let part2 input =
  let antennas = get_antennas input in
  let antinodes : (position, unit) Hashtbl.t = Hashtbl.create 100 in
  (* check if 3 points are in a line *)
  let is_collinear (x1, y1) (x2, y2) (x3, y3) =
    (y2 - y1) * (x3 - x1) = (y3 - y1) * (x2 - x1)
  in
  (* check if [x, y] is an antinode - valid if any pair of antennas collinear *)
  let is_antinode x1 y1 =
    let found = ref false in
    Hashtbl.iter
      (fun (x2, y2) antenna1 ->
        (* early stopping check *)
        if not !found then
          Hashtbl.iter
            (fun (x3, y3) antenna2 ->
              (* check if 2 same antennas exist in a line *)
              if
                antenna1 = antenna2
                && (not ((x2, y2) = (x3, y3))) (* not checking same position *)
                && is_collinear (x1, y1) (x2, y2) (x3, y3)
              then found := true)
            antennas)
      antennas;
    !found
  in
  (* iterate through all positions and check if antinode *)
  List.iteri
    (fun i row ->
      String.iteri
        (fun j _ -> if is_antinode i j then Hashtbl.replace antinodes (i, j) ())
        row)
    input;
  (* count number of antinodes *)
  Hashtbl.length antinodes

let () =
  let input = input_file_to_format file_path in
  Printf.printf "solution to part 1: %d\n" (part1 input);
  Printf.printf "solution to part 2: %d\n" (part2 input)
