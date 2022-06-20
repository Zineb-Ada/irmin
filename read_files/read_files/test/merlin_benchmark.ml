(* let pr_bench value =
   Format.printf
     {|{"results": [{"name": "%s", "metrics": [{"name": "%s", "value": %d, "units": "ms"}]}]}@.|}
     value *)

let pr_bench command_name metrics =
  Format.printf {|{"results": [{"name": "%s", "metrics": [%s]}]}@.|}
    command_name metrics

let metrics metric_name value =
  Printf.sprintf {|{"name": "%s", "value": %d, "units": "ms"}|} metric_name
    value

(* cette fonction est pour rentrer dans les fichiers (channel) qui contienne
   les 100 pires querys et lancé le benchmark dessus
     1- entrer dans les fichiers et lancé le benchmark sur toutes les liges
     2- lancer plusisuers benchmark pour faire un graph pour chacun et
     un graph qui contient les deux versions*)
(* let rec merlin_locate_command l =
   match l with
   | [] -> ()
   | filename :: tl ->
       let channel = open_in filename in
       (try
          let rec benchmark_merlin in_channel =
            let line = input_line channel in
            let line_to_list = String.split_on_char ' ' line in
            match line_to_list with
            | [] -> ()
            | query :: t -> (
                match t with
                | [] -> ()
                | ocamlversion :: _ ->
                    pr_bench "ocamlmerlin_locate"
                      (metrics ocamlversion (int_of_string query));
                    benchmark_merlin in_channel)
          in
          benchmark_merlin channel
          (* close_in channel *)
        with End_of_file -> close_in channel);
       merlin_locate_command tl *)

(* let _ = merlin_locate_command list_of_sorted_query_files *)

let metric_locate filename =
  let accumulator = ref [] in
  let channel = open_in filename in
  try
    let rec metric_locate_4 acc =
      accumulator := acc;
      let line = input_line channel in
      let line_to_list = String.split_on_char ' ' line in

      match line_to_list with
      | [] -> metric_locate_4 acc
      | query :: t -> (
          match t with
          | [] -> acc
          | ocamlversion :: _ ->
              pr_bench "ocamlmerlin_locate" (metrics ocamlversion (int_of_string query));
              metric_locate_4 (metrics ocamlversion (int_of_string query) :: acc));
    in

    metric_locate_4 []
  with End_of_file ->
    close_in channel;
    !accumulator

let rec merlin_locate_command l1 l2 =
  match (l1, l2) with
  | l1, [] -> l1
  | [], l2 -> l2
  | h1 :: t1, h2 :: t2 ->
      pr_bench "ocamlmerlin_locate" (h1 ^ "," ^ h2);
      merlin_locate_command t1 t2

let _ =
  merlin_locate_command
    (metric_locate "read_files/read_files/test/merlin_locate_4_14_0.txt")
    (metric_locate "read_files/read_files/test/merlin_locate_4_12_1.txt")