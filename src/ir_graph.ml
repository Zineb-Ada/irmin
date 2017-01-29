(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt

let src = Logs.Src.create "irmin.graph" ~doc:"Irmin graph support"
module Log = (val Logs.src_log src : Logs.LOG)

let list_partition_map f t =
  let rec aux fst snd = function
    | []   -> List.rev fst, List.rev snd
    | h::t ->
      match f h with
      | `Fst x -> aux (x :: fst) snd t
      | `Snd x -> aux fst (x :: snd) t
  in
  aux [] [] t

module type S = sig
  include Graph.Sig.I
  include Graph.Oper.S with type g := t
  module Topological: sig
    val fold: (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  end
  val vertex: t -> vertex list
  val edges: t -> (vertex * vertex) list
  val closure:
    ?depth:int
    -> pred:(vertex -> vertex list Lwt.t)
    -> min:vertex list
    -> max:vertex list
    -> unit -> t Lwt.t
  val output:
    Format.formatter ->
    (vertex * Graph.Graphviz.DotAttributes.vertex list) list ->
    (vertex * Graph.Graphviz.DotAttributes.edge list * vertex) list ->
    string -> unit
  val min: t -> vertex list
  val max: t -> vertex list
  type dump = vertex list * (vertex * vertex) list
  val export: t -> dump
  val import: dump -> t
  module Dump: Ir_s.S0 with type t = dump
end

module Make
    (Contents: Ir_s.S0)
    (Metadata: Ir_s.S0)
    (Node:     Ir_s.S0)
    (Commit:   Ir_s.S0)
    (Branch:   Ir_s.S0)
= struct

  module X = struct

    type t =
      [ `Contents of Contents.t * Metadata.t
      | `Node of Node.t
      | `Commit of Commit.t
      | `Branch of Branch.t ]

    let t =
      let open Depyt in
      variant "vertex" (fun contents node commit branch -> function
          | `Contents x -> contents x
          | `Node x     -> node x
          | `Commit x   -> commit x
          | `Branch x   -> branch x)
      |~ case1 "contents" (pair Contents.t Metadata.t) (fun x -> `Contents x)
      |~ case1 "node"     Node.t   (fun x -> `Node x)
      |~ case1 "commit"   Commit.t (fun x -> `Commit x)
      |~ case1 "branch"   Branch.t (fun x -> `Branch x)
      |> sealv

    let equal = Depyt.equal t
    let compare = Depyt.compare t
    let hash = Hashtbl.hash
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional(X)
  module GO = Graph.Oper.I(G)
  module Topological = Graph.Topological.Make(G)
  module Table = Hashtbl.Make(X)

  include G
  include GO

  type dump = vertex list * (vertex * vertex) list

  (* XXX: for the binary format, we can use offsets in the vertex list
     to save space. *)
  module Dump = struct
    type t = X.t list * (X.t * X.t) list
    let t = Depyt.(pair (list X.t) (list (pair X.t X.t)))
  end

  let vertex g =
    G.fold_vertex (fun k set -> k :: set) g []

  let edges g =
    G.fold_edges (fun k1 k2 list -> (k1,k2) :: list) g []

  let closure ?(depth=max_int) ~pred ~min ~max () =
    Log.debug (fun f ->
      f "closure depth=%d (%d elements)" depth (List.length max));
    let g = G.create ~size:1024 () in
    let marks = Table.create 1024 in
    let mark key level = Table.add marks key level in
    let has_mark key = Table.mem marks key in
    List.iter (fun k -> mark k max_int) min;
    List.iter (G.add_vertex g) max;
    let todo = Queue.create () in
    List.iter (fun k -> Queue.push (k,0) todo) max;
    let rec add () =
      try
        let (key, level) = Queue.pop todo in
        if level >= depth then add ()
        else if has_mark key then add ()
        else (
          mark key level;
          Log.debug (fun f -> f "ADD %a %d" Depyt.(dump X.t) key level);
          if not (G.mem_vertex g key) then G.add_vertex g key;
          pred key >>= fun keys ->
          List.iter (fun k -> G.add_edge g k key) keys;
          List.iter (fun k -> Queue.push (k, level+1) todo) keys;
          add ()
        )
      with Queue.Empty -> return_unit
    in
    add () >>= fun () ->
    Lwt.return g

  let min g =
    G.fold_vertex (fun v acc ->
        if G.in_degree g v = 0 then v :: acc
        else acc
      ) g []

  let max g =
    G.fold_vertex (fun v acc ->
        if G.out_degree g v = 0 then v :: acc
        else acc
      ) g []

  let vertex_attributes = ref (fun _ -> [])
  let edge_attributes = ref (fun _ -> [])
  let graph_name = ref None
  module Dot = Graph.Graphviz.Dot(struct
      include G
      let edge_attributes k = !edge_attributes k
      let default_edge_attributes _ = []
      let vertex_name k = Fmt.strf "%a" Depyt.(dump X.t) k
      let vertex_attributes k = !vertex_attributes k
      let default_vertex_attributes _ = []
      let get_subgraph _ = None
      let graph_attributes _ =
        match !graph_name with
        | None   -> []
        | Some n -> [`Label n]
    end)

  let export t =
    vertex t, edges t

  let import (vs, es) =
    let g = G.create ~size:(List.length vs) () in
    List.iter (G.add_vertex g) vs;
    List.iter (fun (v1, v2) -> G.add_edge g v1 v2) es;
    g

  let output ppf vertex edges name =
    Log.debug (fun f -> f "output %s" name);
    let g = G.create ~size:(List.length vertex) () in
    List.iter (fun (v,_) -> G.add_vertex g v) vertex;
    List.iter (fun (v1,_,v2) -> G.add_edge g v1 v2) edges;
    let eattrs (v1, v2) =
      try
        let l = List.filter (fun (x,_,y) -> x=v1 && y=v2) edges in
        let l = List.fold_left (fun acc (_,l,_) -> l @ acc) [] l in
        let labels, others =
          list_partition_map (function `Label l -> `Fst l | x -> `Snd x) l
        in
        match labels with
        | []  -> others
        | [l] -> `Label l :: others
        | _   -> `Label (String.concat "," labels) :: others
      with Not_found -> [] in
    let vattrs v =
      try List.assoc v vertex
      with Not_found -> [] in
    vertex_attributes := vattrs;
    edge_attributes := eattrs;
    graph_name := Some name;
    Dot.fprint_graph ppf g

end
