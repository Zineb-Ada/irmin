let exec_command program_command =
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
    
