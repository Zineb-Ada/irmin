(* let exec_command program_command =
     let readme = Unix.open_process_in program_command in
     let rec loop acc =
       try
         let textt = input_line readme in
         loop (textt :: acc)
       with End_of_file -> acc
     in
     List.rev (loop [])

   let exec_shell_command program_command =
     Printf.printf "command: %s \n%!"  program_command;
     let res = exec_command program_command in
     String.concat "\n" res

   (* let exec_shell_command1 program_command =
      let res = exec_command program_command in
      List.iter (Printf.printf "%s \n%!") res *)

   (* let filename = "src/irmin-pack/mem/irmin_pack_mem.ml" *)

   let merlin_shell_command i j filename =
     Printf.sprintf
       "ocamlmerlin single locate -look-for ml -position %d:%d -filename %s < %s \
        | jq ."
       i j filename filename

   (* let find_shell_command out_channel =
     let ch = open_out out_channel in
     Printf.fprintf ch "%s \n%!" (exec_shell_command "find . -name *.ml | grep irmin") *)

   (* let dir_contents dir =
     let _ = find_shell_command
     "/Users/tarides/Desktop/Tarides-Ocaml/irmin/read_files/read_files/test/find_files_ml.txt" in
        let rec loop result = function
          | f :: fs when Sys.is_directory f ->
              Sys.readdir f |> Array.to_list
              (* |> List.filter (fun filename ->
                     if filename = find_shell_command then true else false) *)
              |> List.map (Filename.concat f)
              |> List.append fs |> loop result
          | f :: fs -> loop (f :: result) fs
          | [] -> result
        in
        loop [] [ dir ];; *)

   let cpu f_merlin =
     let json = exec_shell_command f_merlin in
     let parse_json = Yojson.Basic.from_string json in
     match parse_json with
     | `Assoc l -> (
         match List.assoc "timing" l with
         | `Assoc l -> (
             match List.assoc "cpu" l with
             | `Int n -> n
             | _ -> failwith "enexpected cpu's value")
         | _ -> failwith "enexpected cpu")
     | _ -> failwith "enexpected output from ocamlmerlin"

   let longest_cpu_from_one_file in_channel out_channel filename =
     let ch = open_out out_channel in
     (* let i = ref 0 in *)
     let max_cpu = ref (min_int, 0, 0, filename) in
     (try
        let rec longest_cpu acc i =
          let line = input_line in_channel in
          String.iteri
            (fun j c ->
              match c with
              | ' ' | ';' | '.' | ',' | '_' | '=' | ':' | '~' | '(' | ')' | '`'
              | '"' ->
                  ()
              | _ ->
                  let cpu_merlin_command =
                    cpu (merlin_shell_command i j filename)
                  in
                  let acc_cpu, _, _, _ = !acc in
                  if acc_cpu < cpu_merlin_command then
                    acc := (cpu_merlin_command, i, j, filename))
            line;
            (* i := !i + 1; *)
          longest_cpu acc (i + 1)
        in
        longest_cpu max_cpu 0
      with End_of_file -> ());
     let _, i, j, filename = !max_cpu in
     let merlin_command = merlin_shell_command i j filename in
     (* Printf.printf "%d %d %d \n%!" acc_cpu i j;
     Printf.printf "%s \n%!" merlin_command; *)
     Printf.fprintf ch "%s \n%!" merlin_command;
     Printf.fprintf ch "%s \n%!" (exec_shell_command merlin_command)
     (* exec_shell_command merlin_command *)

   (* let stats_from_one_file filename outfile =
     let channel = open_in filename in
     let result = longest_cpu_from_one_file channel outfile in
     (* let result = print_fun channel outfile in *)
     close_in channel;
     result *)
   let longest_cpu_from_all_irmin_files filename out_channel=
     (* find_shell_command filename; *)
     let channel = open_in filename in

     (try
              while true do

               let line = input_line channel in
               let in_channel = open_in line in
               if String.rcontains_from line 2 '_'  then () else
               (* Printf.printf "line %s \n%!" line; *)

               (* stats_from_one_file line out_channel  *)
               longest_cpu_from_one_file in_channel out_channel line;
               (* Printf.printf "coucou \n%!"; *)
               close_in in_channel;

              done;
      with End_of_file -> close_in channel)

     (* Printf.printf "%d \n%!" (List.length (dir_contents
      "read_files/read_files/test/find_files_ml.txt")) *)

   (* let _ =
     stats_from_file filename
       "/Users/tarides/Desktop/Tarides-Ocaml/irmin/read_files/read_files/test/locate_json.txt" *)

   let stats_from_all_files filename outchannel =
     let channel = open_in filename in
     let result = longest_cpu_from_all_irmin_files filename outchannel in
     close_in channel;
     result

   let _ =
     stats_from_all_files "read_files/read_files/test/find_files_ml.txt" "/Users/tarides/Desktop/Tarides-Ocaml/irmin/read_files/read_files/test/locate_json.txt"
*)
(* par la  *)

let exec_command program_command =
  let readme = Unix.open_process_in program_command in
  let rec loop acc =
    try
      let textt = input_line readme in
      (* Printf.printf "%S \n%!" textt; *)
      loop (textt :: acc)
    with End_of_file ->
      ignore (Unix.close_process_in readme);
      acc
  in
  List.rev (loop [])

let exec_shell_command program_command =
  (* Printf.printf "command: %s \n%!" program_command; *)
  let res = exec_command program_command in
  String.concat "\n" res

(* let exec_shell_command1 program_command =
   let res = exec_command program_command in
   List.iter (Printf.printf "%s \n%!") res *)

(* let filename = "src/irmin-pack/mem/irmin_pack_mem.ml" *)

let merlin_shell_command i j filename =
  Printf.sprintf
    "ocamlmerlin single locate -look-for ml -position %d:%d -filename %s < %s \
     | jq ."
    i j filename filename

(* let find_shell_command out_channel =
  let ch = open_out out_channel in
  Printf.fprintf ch "%s \n%!" (exec_shell_command "fd -e ml") *)

(* let dir_contents dir =
   let _ = find_shell_command
   "/Users/tarides/Desktop/Tarides-Ocaml/irmin/read_files/read_files/test/find_files_ml.txt" in
      let rec loop result = function
        | f :: fs when Sys.is_directory f ->
            Sys.readdir f |> Array.to_list
            (* |> List.filter (fun filename ->
                   if filename = find_shell_command then true else false) *)
            |> List.map (Filename.concat f)
            |> List.append fs |> loop result
        | f :: fs -> loop (f :: result) fs
        | [] -> result
      in
      loop [] [ dir ];; *)

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
let cpu f_merlin =
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
(*
   let rec get_last_element l  =
     match l with
       |[] -> None
       |[e] -> Some e
       |_ :: t -> get_last_element t *)

(* let k = ref 1  *)
let rec words_of_line (l : char list) acc_list k =
  (* let k = ref 0 in *)
  (* let the_last (l:string list) = get_last_element l |> Option.to_seq |> String.of_seq in *)
  (* let str = ref "" in *)
  k := !k + 1; 
  match l with

  | [] -> acc_list
  (* |'.' | ' ' | ':' | '=' | '(' | ')' | '[' | ']' | '|' | '+'
     | '-' | '>' | ';' | '*' -> words_of_line *)
  | h :: t -> (
      match h with
      | '.' | ' ' | ':' | '=' | '(' | ')' | '[' | ']' | '|' | '+' | '-' | '>'
      | ';' | '\\' | '/' | '<' | '*'  -> 
          begin 
            match acc_list with 
          |[] -> (words_of_line t ((" ", !k) :: acc_list) k)
          |(hd, _) :: _ -> 
      (* Printf.printf "%d \n%!" !k; *)
          (* let word_col = (List.hd acc_list) in  *)
          if hd = " " then 
          (words_of_line t acc_list k)
          else
          (words_of_line t ((" ", !k) :: acc_list) k)
        end
      | _ -> 
        match acc_list with 
          |[] -> (words_of_line t ((Char.escaped h, !k) :: acc_list) k)
          |(hd, col) :: tl -> 

      (* rajouter un patern matching sur la list avec qui remplace  *)
      
      (* Printf.printf "k: %d \n%!" !k; *)
          if hd = " " then
          (* je rajoute  le fst pour le tuple*)
          
          (words_of_line t ((Char.escaped h, !k) :: tl) k)
          else
            words_of_line t
            (((hd ^ Char.escaped h), col) :: tl) k)

  (* words_of_line l [] [] *)
    let words_list_of_line l =  List.rev (words_of_line l [] (ref 0)) 
    
    (* in
    let reverse_lists = List.rev words_list, List.rev col_list in 
    let combine_lists = List.combine (fst reverse_lists) (snd reverse_lists) in
    combine_lists *)
(* List.rev acc_list *)
(* match acc_list with
   |[] -> None
   |[e] -> e :: acc_list
   |_ :: f -> *)
(* match acc_list with
       |[e] -> if e == String.empty then
   words_of_line t (Char.escaped h :: acc_list)
   else words_of_line t (Char.escaped h :: acc_list)
       |_ -> []*)

(* let rec hh =

        match acc_list with
            |[] -> !str
            |d :: f ->
              match d with
              |" " -> words_of_line t acc_list
              |_ -> (ignore (words_of_line t ((Char.escaped h ^ d) :: acc_list))) *)
(* match acc_list with
    |[] -> words_of_line t acc_list
    |d :: _ ->
      match d with
      |" " -> words_of_line t acc_list
      |_ -> (ignore (words_of_line t ((Char.escaped h ^ d) :: acc_list))); acc_list
      in *)
(* words_of_line split_string [] in *)

(* let list_to_seq = List.to_seq split_string in
   let strong_of_seq = String.of_seq list_to_seq in *)


let longest_cpu_from_one_file in_channel filename out_channel =
  let ch = open_out out_channel in
  (* let i = ref 0 in *)
  let max_cpu = ref (min_int, 0, 0, filename) in
  (try
     let rec longest_cpu acc i =
       let line = input_line in_channel in
       let split_string = String.to_seq line |> List.of_seq in
       (* List.iter (Printf.printf " %c") (split_string); *)
       let newline = words_list_of_line split_string  in
       (* Printf.printf "line : %s" "(fst (List.split newline))"; *)
       (* let k_list = snd (List.split newline) in *)
       (* let j = ref 1 in  *)
       List.iter
         (fun (c, k) ->
           match c with
           (* | " " | "." | "(" | ")" | "" | "/" | "+" | ":" | "=" | "->" | "<"
           | ";" | "*" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
           | "0"  *)
           | "let" | "type" | "rec" | "in" | " " ->
               () (* | "," | ";" | "" | "" | "" -> () *)
           | _ ->
               (* if c == "(*" then ()  *)
               (* Printf.printf "c: %S, k: %d \n%!" c k; *)
               let cpu_merlin_command =
                 cpu (merlin_shell_command i k filename)
               in
               let acc_cpu, _, _, _ = !acc in
               Printf.printf "%d i : %d,  j : %d filename: %s \n%!" cpu_merlin_command
                 i k filename;
               if acc_cpu < cpu_merlin_command then
                 acc := (cpu_merlin_command, i, k, filename)
         )
         (newline);
       (* i := !i + 1; *)
       longest_cpu acc (i + 1)
     in
     longest_cpu max_cpu 1
   with End_of_file -> ());
  let acc_cpu, i, k, filename = !max_cpu in
  let merlin_command = merlin_shell_command i k filename in
  (* Printf.printf "%d %d %d \n%!" acc_cpu i j;
     Printf.printf "%s \n%!" merlin_command; *)
  (* Printf.fprintf ch "%s \n%!" merlin_command;
     Printf.fprintf ch "%s \n%!" (exec_shell_command merlin_command); *)
     Printf.fprintf ch "%s \n%!" merlin_command;
     Printf.fprintf ch "%s \n%!" (exec_shell_command merlin_command);
 
  (acc_cpu, i, k, filename)
(* exec_shell_command merlin_command *)
(* let stats_from_one_file filename outfile =
   let channel = open_in filename in
   let result = longest_cpu_from_one_file channel outfile in
   (* let result = print_fun channel outfile in *)
   close_in channel;
   result *)
let longest_cpu_from_all_ml_irmin_files filename out_channel =
  (* find_shell_command filename; *)
  let ch = open_out out_channel in
  let channel = open_in filename in
  let max_cpu = ref (min_int, 0, 0, "") in
  (* let cpu_from_one_file in_channel out_channel line = longest_cpu_from_one_file in_channel out_channel line in *)
  try
    (* while true do *)
    let rec longest_cpu_in_all_files acc =
      let line = input_line channel in
      let in_channel = open_in line in
      (* Printf.printf ("%b") (String.rcontains_from line 2 '_'); *)
      (* if String.rcontains_from line 2 '_' then ()
         else *)
      let cpu_from_one_file = longest_cpu_from_one_file in_channel line out_channel in
      let acc_cpu_acc, _, _, _ = !acc in
      let acc_cpu, _, _, line = cpu_from_one_file in
      let _ = line in
      if acc_cpu_acc < acc_cpu then
        (* Printf.printf "cpu_acc:  %d \n%!"  acc_cpu_acc; *)
        acc := cpu_from_one_file;

      (* Printf.printf "line %s \n%!" line; *)
      (* stats_from_one_file line out_channel  *)
      (* cpu_from_one_file in_channel out_channel line; *)
      (* Printf.printf "coucou \n%!"; *)
      close_in in_channel;
      longest_cpu_in_all_files acc
    in
    longest_cpu_in_all_files max_cpu
  with End_of_file ->
    close_in channel;
    let _, i, k, line = !max_cpu in
    let merlin_command = merlin_shell_command i k line in
    (* Printf.printf "%d %d %d \n%!" acc_cpu i j;
       Printf.printf "%s \n%!" merlin_command; *)
    Printf.fprintf ch "%s \n%!" merlin_command;
    Printf.fprintf ch "%s \n%!" (exec_shell_command merlin_command)

(* Printf.printf "%d \n%!" (List.length (dir_contents
   "read_files/read_files/test/find_files_ml.txt")) *)

(* let _ =
   stats_from_file filename
     "/Users/tarides/Desktop/Tarides-Ocaml/irmin/read_files/read_files/test/locate_json.txt" *)

let stats_from_all_files filename outchannel =
  let channel = open_in filename in
  let result = longest_cpu_from_all_ml_irmin_files filename outchannel in
  close_in channel;
  result

let _ =
  stats_from_all_files "read_files/read_files/test/find_files_ml.txt"
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