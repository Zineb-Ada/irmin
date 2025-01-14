module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "value_unit" (void @-> returning value) (fun () -> Root.create_value ())

  let () =
    fn "value_int"
      (int64_t @-> returning value)
      (fun i -> Root.create_value (Int64.to_int i))

  let () =
    fn "value_float" (double @-> returning value) (fun i -> Root.create_value i)

  let () =
    fn "value_bool" (bool @-> returning value) (fun b -> Root.create_value b)

  let () =
    fn "value_clone"
      (value @-> returning value)
      (fun x -> Root.create_value (Root.get_value x))

  let () =
    fn "realloc"
      (ptr void @-> ptr void @-> returning (ptr void))
      (fun src dest ->
        Ctypes.Root.set src (Ctypes.Root.get dest);
        src)

  let () =
    fn "value_get_string"
      (value @-> returning irmin_string)
      (fun value ->
        let obj = Root.get_value value |> Obj.repr in
        if Obj.tag obj = Obj.string_tag then Root.create_string (Obj.obj obj)
        else null irmin_string)

  let () =
    fn "value_get_int"
      (value @-> returning int64_t)
      (fun x ->
        let obj = Root.get_value x |> Obj.repr in
        if Obj.is_int obj then Int64.of_int (Obj.obj obj) else Int64.zero)

  let () =
    fn "value_get_bool"
      (value @-> returning bool)
      (fun x ->
        let obj = Root.get_value x |> Obj.repr in
        if Obj.is_int obj then Obj.obj obj else false)

  let () =
    fn "value_get_float"
      (value @-> returning double)
      (fun x ->
        let obj = Root.get_value x |> Obj.repr in
        if Obj.is_int obj then Obj.obj obj else 0.)

  let () =
    fn "value_bytes"
      (ptr char @-> int64_t @-> returning value)
      (fun s length ->
        let length = get_length length s in
        Root.create_value (Bytes.of_string (string_from_ptr s ~length)))

  let () =
    fn "value_string"
      (ptr char @-> int64_t @-> returning value)
      (fun s length ->
        let length = get_length length s in
        Root.create_value (string_from_ptr s ~length))

  let () =
    fn "value_array"
      (ptr value @-> uint64_t @-> returning value)
      (fun arr n ->
        let n = UInt64.to_int n in
        let a =
          if is_null arr || n = 0 then [||]
          else
            CArray.from_ptr arr n
            |> CArray.to_list
            |> List.map Root.get_value
            |> Array.of_list
        in
        Root.create_value a)

  let () =
    fn "value_list"
      (ptr value @-> uint64_t @-> returning value)
      (fun arr n ->
        let n = UInt64.to_int n in
        let l =
          if is_null arr || n = 0 then []
          else
            CArray.from_ptr arr n |> CArray.to_list |> List.map Root.get_value
        in
        Root.create_value l)

  let () =
    fn "value_option"
      (value @-> returning value)
      (fun value ->
        if is_null value then Root.create_value None
        else
          let x = Root.get_value value in
          Root.create_value (Some x))

  let () =
    fn "value_pair"
      (value @-> value @-> returning value)
      (fun a b ->
        let a = Root.get_value a in
        let b = Root.get_value b in
        Root.create_value (a, b))

  let () =
    fn "value_triple"
      (value @-> value @-> value @-> returning value)
      (fun a b c ->
        let a = Root.get_value a in
        let b = Root.get_value b in
        let c = Root.get_value c in
        Root.create_value (a, b, c))

  let () =
    fn "value_to_string"
      (ty @-> value @-> returning irmin_string)
      (fun ty value ->
        let t = Root.get_ty ty in
        let v = Root.get_value value in
        let s = Irmin.Type.to_string t v in
        Root.create_string s)

  let () =
    fn "value_of_string"
      (ty @-> ptr char @-> int64_t @-> returning value)
      (fun ty s length ->
        let length = get_length length s in
        let ty = Root.get_ty ty in
        let s = string_from_ptr s ~length in
        match Irmin.Type.(of_string ty) s with
        | Ok x -> Root.create_value x
        | Error (`Msg _) -> null value)

  let () =
    fn "value_to_bin"
      (ty @-> value @-> returning irmin_string)
      (fun ty v ->
        let t = Root.get_ty ty in
        let v = Root.get_value v in
        let s = Irmin.Type.(unstage (to_bin_string t)) v in
        Root.create_string s)

  let () =
    fn "value_of_bin"
      (ty @-> ptr char @-> int64_t @-> returning value)
      (fun ty s length ->
        let length = get_length length s in
        let ty = Root.get_ty ty in
        let s = string_from_ptr s ~length in
        match Irmin.Type.(unstage (of_bin_string ty)) s with
        | Ok x -> Root.create_value x
        | Error (`Msg _) -> null value)

  let () =
    fn "value_to_json"
      (ty @-> value @-> returning irmin_string)
      (fun ty v ->
        let t = Root.get_ty ty in
        let v = Root.get_value v in
        let s = Irmin.Type.(to_json_string t) v in
        Root.create_string s)

  let () =
    fn "value_of_json"
      (ty @-> ptr char @-> int64_t @-> returning value)
      (fun ty s length ->
        let length = get_length length s in
        let ty = Root.get_ty ty in
        let s = string_from_ptr s ~length in
        match Irmin.Type.(of_json_string ty) s with
        | Ok x -> Root.create_value x
        | Error (`Msg _) -> null value)

  let () =
    fn "value_equal"
      (ty @-> value @-> value @-> returning bool)
      (fun ty a b ->
        let ty = Root.get_ty ty in
        let a = Root.get_value a in
        let b = Root.get_value b in
        Irmin.Type.(unstage (equal ty)) a b)

  let () =
    fn "value_compare"
      (ty @-> value @-> value @-> returning int)
      (fun ty a b ->
        let ty = Root.get_ty ty in
        let a = Root.get_value a in
        let b = Root.get_value b in
        Irmin.Type.(unstage (compare ty)) a b)

  let () = fn "value_free" (value @-> returning void) free

  let () =
    fn "string_new"
      (ptr char @-> int64_t @-> returning irmin_string)
      (fun ptr i ->
        let i = Int64.to_int i in
        let length = if i < 0 then strlen ptr else i in
        let s = string_from_ptr ptr ~length in
        Root.create_string s)

  let () =
    fn "string_data"
      (irmin_string @-> returning (ptr char))
      (fun s ->
        if is_null s then null (ptr char)
        else
          let s : string = Root.get_string s in
          coerce string (ptr char) s)

  let () =
    fn "string_length"
      (irmin_string @-> returning uint64_t)
      (fun s ->
        if is_null s then UInt64.zero
        else
          let s : string = Root.get_string s in
          String.length s |> UInt64.of_int)

  let () = fn "string_free" (irmin_string @-> returning void) free
end
