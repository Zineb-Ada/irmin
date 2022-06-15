let exec_command program_command =
  let readme = Unix.open_process_in program_command in
  let rec loop acc =
    try
      let textt = input_line readme in
      loop (textt :: acc)
    with End_of_file ->
      ignore (Unix.close_process_in readme);
      acc
  in
  List.rev (loop [])

let exec_shell_command program_command =
  let res = exec_command program_command in
  String.concat "\n" res

let merlin_shell_command i j filename =
  Printf.sprintf
    "ocamlmerlin single locate -look-for ml -position %d:%d -filename %s < %s \
     | jq ."
    i j filename filename

(* for cpu *)
(* let cpu f_merlin =
  let json = exec_shell_command f_merlin in
  (* Printf.printf "json : %S \n%!" json; *)
  let parse_json = Yojson.Basic.from_string json in
  match parse_json with
  | `Assoc l -> (
      match List.assoc "timing" l with
      | `Assoc l -> (
          match List.assoc "cpu" l with
          | `Int n -> n
          | _ -> failwith "enexpected cpu's value")
      | _ -> failwith "enexpected cpu")
  | _ -> failwith "enexpected output from ocamlmerlin" *)

(* for query *)
let query f_merlin =
  let json = exec_shell_command f_merlin in
  (* Printf.printf "json : %S \n%!" json; *)
  let parse_json = Yojson.Basic.from_string json in
  match parse_json with
  | `Assoc l -> (
      match List.assoc "timing" l with
      | `Assoc l -> (
          match List.assoc "query" l with
          | `Int n -> n
          | _ -> failwith "enexpected query's value")
      | _ -> failwith "enexpected query")
  | _ -> failwith "enexpected output from ocamlmerlin"

let rec words_of_line (l : char list) acc_list k =
  k := !k + 1; 
  match l with

  | [] -> acc_list
  | h :: t -> (
      match h with
      | '.' | ' ' | ':' | '=' | '(' | ')' | '[' | ']' | '|' | '+' | '-' | '>'
      | ';' | '\\' | '/' | '<' | '*'  -> 
          begin 
            match acc_list with 
          |[] -> (words_of_line t ((" ", !k) :: acc_list) k)
          |(hd, _) :: _ -> 
          if hd = " " then 
          (words_of_line t acc_list k)
          else
          (words_of_line t ((" ", !k) :: acc_list) k)
        end
      | _ -> 
        match acc_list with 
          |[] -> (words_of_line t ((Char.escaped h, !k) :: acc_list) k)
          |(hd, col) :: tl -> 
          if hd = " " then          
          (words_of_line t ((Char.escaped h, !k) :: tl) k)
          else
            words_of_line t
            (((hd ^ Char.escaped h), col) :: tl) k)
    let words_list_of_line l =  List.rev (words_of_line l [] (ref 0)) 
    
let longest_query_of_one_file in_channel filename out_channel =
  let ch = open_out out_channel in
  let max_query = ref (min_int, 0, 0, filename) in
  (try
     let rec longest_query acc i =
       let line = input_line in_channel in
       let split_string = String.to_seq line |> List.of_seq in
       let newline = words_list_of_line split_string  in
       List.iter
         (fun (c, k) ->
           match c with
           | "let" | "type" | "rec" | "in" | " " ->
               ()
           | _ ->
               let query_merlin_command =
                  query (merlin_shell_command i k filename)
               in
               let acc_query, _, _, _ = !acc in
               Printf.printf "query: %d, i : %d,  j : %d filename: %s \n%!" acc_query
                 i k filename;
               if acc_query < query_merlin_command then
                 acc := (query_merlin_command, i, k, filename)
         )
         (newline);
       longest_query acc (i + 1)
     in
     longest_query max_query 1
   with End_of_file -> ());
  let acc_query, i, k, filename = !max_query in
  let merlin_command = merlin_shell_command i k filename in
  (* Printf.printf "%d %d %d \n%!" acc_query i j;
     Printf.printf "%s \n%!" merlin_command; *)
  (* Printf.fprintf ch "%s \n%!" merlin_command;
     Printf.fprintf ch "%s \n%!" (exec_shell_command merlin_command); *)
     Printf.fprintf ch "%s \n%!" merlin_command;
     Printf.fprintf ch "%s \n%!" (exec_shell_command merlin_command);
  (acc_query, i, k, filename)
let longest_query_of_all_ml_irmin_files filename out_channel =
  (* find_shell_command filename; *)
  let ch = open_out out_channel in
  let channel = open_in filename in
  let max_query = ref (min_int, 0, 0, "") in
  try
    let rec longest_query_in_all_files acc =
      let line = input_line channel in
      let in_channel = open_in line in
      let query_of_one_file = longest_query_of_one_file in_channel line out_channel in
      let acc_query_acc, _, _, _ = !acc in
      let acc_query, _, _, line = query_of_one_file in
      let _ = line in
      if acc_query_acc < acc_query then
        acc := query_of_one_file;

      (* Printf.printf "line %s \n%!" line; *)
      (* stats_from_one_file line out_channel  *)
      (* query_from_one_file in_channel out_channel line; *)
      (* Printf.printf "coucou \n%!"; *)
      close_in in_channel;
      longest_query_in_all_files acc
    in
    longest_query_in_all_files max_query
  with End_of_file ->
    close_in channel;
    let _, i, k, line = !max_query in
    let merlin_command = merlin_shell_command i k line in
    (* Printf.printf "%d %d %d \n%!" acc_query i j;
       Printf.printf "%s \n%!" merlin_command; *)
    Printf.fprintf ch "%s \n%!" merlin_command;
    Printf.fprintf ch "%s \n%!" (exec_shell_command merlin_command)
let stats_of_all_files filename outchannel =
  let channel = open_in filename in
  let result = longest_query_of_all_ml_irmin_files filename outchannel in
  close_in channel;
  result

let _ =
  stats_of_all_files "read_files/read_files/test/find_files_ml.txt"
    "/Users/tarides/Desktop/Tarides-Ocaml/irmin/read_files/read_files/test/locate_json.txt"


(* ocaml 4.14.0 ocamlmerlin single locate -look-for ml -position 20:16 -filename ./examples/trees.ml < ./examples/trees.ml | jq . 
{
  "class": "return",
  "value": {
    "file": "/Users/tarides/Desktop/Tarides-Ocaml/irmin/src/irmin-unix/irmin_unix.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  },
  "notifications": [],
  "timing": {
    "clock": 1370,
    "cpu": 1355,
    "query": 1149,
    "pp": 0,
    "reader": 1,
    "ppx": 22,
    "typer": 183,
    "error": 0
  }
} 
 *)

 (* ocaml 4.12.1  
    ocamlmerlin single locate -look-for ml -position 20:16 -filename ./examples/trees.ml < ./examples/trees.ml | jq . 
{
  "class": "return",
  "value": {
    "file": "/Users/tarides/Desktop/Tarides-Ocaml/irmin/src/irmin-unix/irmin_unix.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  },
  "notifications": [],
  "timing": {
    "clock": 441,
    "cpu": 321,
    "query": 73,
    "pp": 0,
    "reader": 1,
    "ppx": 25,
    "typer": 223,
    "error": 0
  }
}
➜  irmin git:(main) ✗ *)