let file_path = "data/day6.txt"

(* create types for direction and position *)
type direction = Up | Right | Down | Left
type position = int * int
type state = position * direction

(** convert guard position to direction. if guard not found, return [None] *)
let direction_of_char = function
  | '^' -> Some Up
  | '>' -> Some Right
  | 'v' -> Some Down
  | '<' -> Some Left
  | _ -> None

(** convert direction to movement vector *)
let get_direction_vector = function
  | Up -> (0, -1)
  | Right -> (1, 0)
  | Down -> (0, 1)
  | Left -> (-1, 0)

(** turn right direction conversion *)
let turn_right = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

(** find starting position and direction *)
let find_start grid =
  let height = List.length grid in
  let width = String.length (List.hd grid) in
  let rec find_guard y =
    if y >= height then None
    else
      let row = List.nth grid y in
      let rec find_in_row x =
        (* if not found in row, move to next *)
        if x >= width then find_guard (y + 1)
        else
          match direction_of_char row.[x] with
          (* if direction found, return *)
          | Some dir -> Some ((x, y), dir)
          (* if not found check next cell *)
          | None -> find_in_row (x + 1)
      in
      find_in_row 0
  in
  find_guard 0

(** check if position is within grid *)
let is_valid_pos (x, y) grid =
  let height = List.length grid in
  let width = String.length (List.hd grid) in
  x >= 0 && x < width && y >= 0 && y < height

(** get next position based on current position and direction *)
let get_next_position (x, y) dir =
  let dx, dy = get_direction_vector dir in
  (x + dx, y + dy)

(* module for position sets *)
module PosSet = Set.Make (struct
  type t = position

  let compare = compare
end)

(* module for state sets *)
module StateSet = Set.Make (struct
  type t = state

  let compare = compare
end)

(** part 1 *)
let part1 grid =
  match find_start grid with
  | None -> 0
  | Some (start_pos, start_dir) ->
      let rec walk pos dir visited =
        let next_pos = get_next_position pos dir in
        (* if next pos is outside grid, return number of visited *)
        if not (is_valid_pos next_pos grid) then PosSet.cardinal visited
        else
          let x, y = (fst next_pos, snd next_pos) in
          let curr_row = List.nth grid y in
          (* if obstacle, turn right and continue walk *)
          if curr_row.[x] = '#' then walk pos (turn_right dir) visited
            (* otherwise walk from next pos and add next pos to list of visited *)
          else walk next_pos dir (PosSet.add next_pos visited)
      in
      walk start_pos start_dir (PosSet.singleton start_pos)

(** part 2 *)
let part2 grid =
  let height = List.length grid in
  let width = String.length (List.hd grid) in

  match find_start grid with
  | None -> 0
  | Some ((start_x, start_y), start_dir) ->
      (* place obstacle at [x], [y] in grid and returns new grid *)
      let modify_grid x y =
        List.mapi
          (fun yi row ->
            if yi = y then
              String.mapi (fun xi c -> if xi = x then '#' else c) row
            else row)
          grid
      in

      (* checks if an obstacle at position [x], [y] creates a loop *)
      let try_position x y =
        (* if position is not empty or starting position then return false *)
        if
          List.nth grid y |> (fun row -> row.[x]) <> '.'
          || (x = start_x && y = start_y)
        then false
        else
          (* new grid with obstacle at [x], [y] *)
          let new_grid = modify_grid x y in
          let rec walk pos dir visited path_length =
            let state = (pos, dir) in
            (* if same state has been visited it's looping *)
            if StateSet.mem state visited then true
              (* inf loop prevention using max possible states - not sure if needed but guardrail *)
            else if path_length > width * height * 4 then false
            else
              let next_pos = get_next_position pos dir in
              (* if guard exits, it's not looping *)
              if not (is_valid_pos next_pos new_grid) then false
              else
                let nx, ny = (fst next_pos, snd next_pos) in
                let curr_row = List.nth new_grid ny in
                (* if next pos is obstacle, turn and continue from current pos *)
                if curr_row.[nx] = '#' then
                  walk pos (turn_right dir)
                    (StateSet.add state visited)
                    (path_length + 1)
                else
                  walk next_pos dir
                    (StateSet.add state visited)
                    (path_length + 1)
          in
          walk (start_x, start_y) start_dir StateSet.empty 0
      in

      (* count possible obstacle possitions *)
      let rec count_valid_positions x y acc =
        if y >= height then acc
        else if x >= width then count_valid_positions 0 (y + 1) acc
        else
          let new_acc = if try_position x y then acc + 1 else acc in
          count_valid_positions (x + 1) y new_acc
      in

      count_valid_positions 0 0 0

(** input file to list of strings that's used as grid *)
let input_file_to_array file_path =
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  String.split_on_char '\n' content
  |> List.filter (fun x -> String.length x > 1)

let () =
  let grid = input_file_to_array file_path in
  Printf.printf "solution to part 1: %d\n" (part1 grid);
  Printf.printf "solution to part 2: %d\n" (part2 grid)
