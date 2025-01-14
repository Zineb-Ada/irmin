open Import
open Irmin_pack.Inode

module type Persistent = sig
  include S

  type index

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    indexing_strategy:Irmin_pack.Indexing_strategy.t ->
    string ->
    read t Lwt.t

  include Irmin_pack.S.Checkable with type 'a t := 'a t and type hash := hash

  val sync : 'a t -> unit
  val clear_caches : 'a t -> unit
  val integrity_check_inodes : [ `Read ] t -> key -> (unit, string) result Lwt.t

  module Pack :
    Pack_store.S
      with type index := index
       and type key := hash Pack_key.t
       and type hash := hash
       and type 'a t = 'a t

  module Raw :
    Raw
      with type t = Pack.value
       and type hash := hash
       and type key := hash Pack_key.t

  module Snapshot :
    Snapshot with type hash = hash and type metadata = Val.metadata

  val to_snapshot : Raw.t -> Snapshot.inode
  val of_snapshot : 'a t -> index:(hash -> key) -> Snapshot.inode -> value
end

module type Sigs = sig
  module type S = S
  module type Persistent = Persistent

  module Make_persistent
      (H : Irmin.Hash.S)
      (Node : Irmin.Node.Generic_key.S
                with type hash = H.t
                 and type contents_key = H.t Pack_key.t
                 and type node_key = H.t Pack_key.t)
      (Inter : Internal
                 with type hash = H.t
                  and type key = H.t Pack_key.t
                  and type Snapshot.metadata = Node.metadata
                  and type Val.step = Node.step)
      (CA : Pack_store.Maker
              with type hash = H.t
               and type index := Pack_index.Make(H).t) :
    Persistent
      with type key = H.t Pack_key.t
       and type hash = H.t
       and type Val.metadata = Node.metadata
       and type Val.step = Node.step
       and type index := Pack_index.Make(H).t
       and type value = Inter.Val.t
end
