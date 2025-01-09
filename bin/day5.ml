let file_path = "data/day5.txt"

(** input file to the rules and updates format *)
let input_file_to_format file_path =
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

(** update status based on whether a and b are in correct order in lst *)
let new_status a b lst =
  match
    ( List.find_index (fun x -> x == a) lst,
      List.find_index (fun x -> x == b) lst )
  with
  | Some a, Some b -> a < b
  | _ -> true

(** function to get mid point of correctly sorted updates *)
let rec update_result update rules status =
  match rules with
  | [] when status -> List.nth update (List.length update / 2)
  | (a, b) :: rest when status ->
      update_result update rest (new_status a b update)
  | _ -> 0

(** create topo sorting of all available rules *)
let topological_sort rules =
  (* initialise random sized tables *)
  let adj_list = Hashtbl.create 10 in
  let in_degree = Hashtbl.create 10 in

  (* create graph and calc in degrees *)
  List.iter
    (fun (a, b) ->
      Hashtbl.replace adj_list a
        (b :: (Hashtbl.find_opt adj_list a |> Option.value ~default:[]));
      Hashtbl.replace adj_list b
        (Hashtbl.find_opt adj_list b |> Option.value ~default:[]);
      Hashtbl.replace in_degree a
        (Hashtbl.find_opt in_degree a |> Option.value ~default:0);
      Hashtbl.replace in_degree b
        ((Hashtbl.find_opt in_degree b |> Option.value ~default:0) + 1))
    rules;

  (* add nodes with no in-degrees to queue *)
  let queue = Queue.create () in
  Hashtbl.iter (fun node deg -> if deg = 0 then Queue.add node queue) in_degree;

  (* kahn's algo *)
  let rec kahn_algo queue sorted =
    if Queue.is_empty queue then List.rev sorted (* return final sorted list *)
    else
      let node = Queue.pop queue in
      let neighbors =
        Hashtbl.find_opt adj_list node |> Option.value ~default:[]
      in
      (* update in degrees and add to queue if no more in-degrees *)
      List.iter
        (fun neighbor ->
          let updated_deg = Hashtbl.find in_degree neighbor - 1 in
          Hashtbl.replace in_degree neighbor updated_deg;
          if updated_deg = 0 then Queue.add neighbor queue)
        neighbors;
      (* rec call with new queue and so-far-sorted list *)
      kahn_algo queue (node :: sorted)
  in
  kahn_algo queue []

(** function to get mid point of incorrect updates after correctly sorting them *)
let incorrrect_update_result update rules =
  (* first filter all rules based on ones that are required to sort update and then topo order only those *)
  let topo_sorted_rules =
    List.filter (fun (a, b) -> List.mem a update && List.mem b update) rules
    |> topological_sort
  in
  let find_correct_mid update sorted_rules =
    (* interesting way to sort the update by filtering topo sorted ruleset *)
    let correctly_sorted_update =
      List.filter (fun x -> List.mem x update) sorted_rules
    in
    (* mid elem in sorted list *)
    List.nth correctly_sorted_update (List.length correctly_sorted_update / 2)
  in
  if update_result update rules true == 0 then
    find_correct_mid update topo_sorted_rules
  else 0

let () =
  let rules, updates = input_file_to_format file_path in

  (* part 1 *)
  let output_p1 =
    List.fold_left
      (fun acc update -> acc + update_result update rules true)
      0 updates
  in

  (* part 2 *)
  let output_p2 =
    List.fold_left
      (fun acc update -> acc + incorrrect_update_result update rules)
      0 updates
  in
  Printf.printf "\nsolution to part 1: %d" output_p1;
  Printf.printf "\nsolution to part 2: %d\n" output_p2
