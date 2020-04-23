type ('a, 'u) typ = {
  read: 'u Value.input -> 'a;
  write: 'u Value.output -> 'a -> unit;
  traverse: 'acc. (('acc, 'u) traversal -> 'acc -> 'a -> 'acc) option;
}

and ('acc, 'u) traversal = {
  step: 'k 'a. 'acc -> ('a, 'u) typ -> ('k, 'a, 'u) Block.t -> 'acc;
} [@@ocaml.unboxed]

let int32 = {
  read = Value.input_int32_le;
  write = Value.output_int32_le;
  traverse = None;
}

let unit = {
  read = (fun _ -> ());
  write = (fun _ () -> ());
  traverse = None;
}

let input_varint_32 inp =
  let result = Value.input_uint8 inp in
  if result = 0xFF
  then Int32.to_int (Value.input_int32_be inp)
  else result

let output_varint_32 out n =
  if n < 0xFE then
    Value.output_uint8 out n
  else (
    Value.output_uint8 out 0xFF;
    Value.output_int32_be out (Int32.of_int n);
  )

let string = {
  read = (fun inp ->
      let length = input_varint_32 inp in
      let text = Value.input_string inp length in
      text
    );
  write = (fun out text ->
      let length = String.length text in
      output_varint_32 out length;
      Value.output_substring out text ~offset:0 ~length
    );
  traverse = None;
}

let option {read; write; traverse} = {
  read = (fun inp ->
      match Value.input_int8 inp with
      | 0 -> None
      | 1 -> Some (read inp)
      | _ -> failwith "Typed.option: corrupt data"
    );
  write = (fun out -> function
      | None -> Value.output_int8 out 0
      | Some v ->
        Value.output_int8 out 1;
        write out v
    );
  traverse = (match traverse with
      | None -> None
      | Some f -> Some (fun traversal acc -> function
          | None -> acc
          | Some v -> f traversal acc v
        )
    );
}

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let either
    {read=lread; write=lwrite; traverse=ltraverse}
    {read=rread; write=rwrite; traverse=rtraverse}
  = {
    read = (fun inp ->
        match Value.input_int8 inp with
        | 0 -> Left (lread inp)
        | 1 -> Right (rread inp)
        | _ -> failwith "Typed.option: corrupt data"
      );
    write = (fun out -> function
        | Left v ->
          Value.output_int8 out 0;
          lwrite out v
        | Right v ->
          Value.output_int8 out 1;
          rwrite out v
      );
    traverse = match ltraverse, rtraverse with
      | None, None -> None
      | Some l, None -> Some (fun traversal acc -> function
          | Left v -> l traversal acc v
          | Right _ -> acc
        )
      | None, Some r -> Some (fun traversal acc -> function
          | Left _ -> acc
          | Right v -> r traversal acc v
        )
      | Some l, Some r -> Some (fun traversal acc -> function
          | Left v -> l traversal acc v
          | Right v -> r traversal acc v
        )
  }

let pair
    {read=lread; write=lwrite; traverse=ltraverse}
    {read=rread; write=rwrite; traverse=rtraverse}
  = {
    read = (fun inp ->
        let l = lread inp in
        let r = rread inp in
        (l, r)
      );
    write = (fun out (l, r) ->
        lwrite out l;
        rwrite out r
      );
    traverse = match ltraverse, rtraverse with
      | None, None -> None
      | Some l, None -> Some (fun traversal acc (v,_) -> l traversal acc v)
      | None, Some r -> Some (fun traversal acc (_,v) -> r traversal acc v)
      | Some l, Some r -> Some (fun traversal acc (vl,vr) ->
          r traversal (l traversal acc vl) vr)
  }

let bimap ab ba { read; write; traverse } = {
  read = (fun inp -> ab (read inp));
  write = (fun out v -> write out (ba v));
  traverse = match traverse with
    | None -> None
    | Some f -> Some (fun traversal acc v -> f traversal acc (ba v))
}

let addr (type k) role (family : (k, 'u) Address.family) typ =
  {
    read = (fun inp ->
        let role', t = Value.input_ref inp family in
        if role <> role' then
          Printf.ksprintf failwith
            "Typed.addr.read: role mismatch (got %S, expecting %S)"
            role' role;
        Block.in_store family (Value.input_store inp) t typ.read
      );
    write = (fun out v ->
        match v.address with
        | None ->
          (* Write expects that the object has already been traversed and
             that addresses/hashes are computed. *)
          failwith "Typed.addr.write: address has not been \
                    generated (store misbehaved)"
        | Some h -> Value.output_ref out family role h
      );
    traverse = Some (fun traversal acc v -> traversal.step acc typ v);
  }

type ('a, 'u) recursive = {
  final: ('a, 'u) typ;
  def: ('a, 'u) typ ref;
}

let fail_type _ = failwith "Unclosed recursive type"

let invalid_type = { read = fail_type; write = fail_type; traverse = None }

let recursion () =
  let def = ref invalid_type in
  let final = {
    read = (fun inp -> !def.read inp);
    write = (fun out v -> !def.write out v);
    traverse = Some (fun traversal acc v ->
        match !def.traverse with
        | None -> acc
        | Some f -> f traversal acc v
      );
  } in
  { final; def }

let recurse r = r.final

let seal r t =
  if r.def.contents != invalid_type
  then failwith "Recursive type already sealed"
  else (
    r.def.contents <- t;
    r.final
  )

type ('k, 'a, 'u) ilist =
  | Nil
  | Cons of 'a * ('k, ('k, 'a, 'u) ilist, 'u) Block.t

let ilist (type k a u)
    (f : (k, u) Address.family) (t : (a, u) typ) : ((k, a, u) ilist, u) typ =
  let typ = recursion () in
  seal typ (
    bimap
      (function None -> Nil | Some (a, b) -> Cons (a, b))
      (function Nil -> None | Cons (a, b) -> Some (a, b))
      (option (pair t (addr "" f (recurse typ))))
  )

type 'u store_queue =
  | Done
  | Todo : {
      typ: ('a, 'u) typ;
      block : ('k, 'a, 'u) Block.t;
      next : 'u store_queue;
    } -> 'u store_queue

let store st typ block =
  let queue =
    (* Compute work queue *)
    let traversal = { step = fun todo typ block ->
        match block.address with
        | None -> Todo { typ; block; next = todo }
        | Some _ -> todo
      }
    in
    let rec expand_queue acc tail = function
      | u when u == tail -> acc
      | Done -> assert false
      | Todo t ->
        let value = match t.block.value with
          | Block.In_store _ -> assert false
          | Block.In_memory v -> v
        in
        let acc = match t.typ.traverse with
          | None -> acc
          | Some f -> f traversal acc value
        in
        expand_queue acc tail t.next
    in
    let rec expand_full queue tail =
      let queue' = expand_queue queue tail queue in
      if queue' != queue
      then expand_full queue' queue
      else queue'
    in
    expand_full (Todo {typ; block; next = Done}) Done
  in
  let rec pump = function
    | Done -> Lwt.return_ok ()
    | Todo t ->
      let value = match t.block.value with
        | Block.In_store _ -> assert false
        | Block.In_memory v -> v
      in
      Lwt.bind (Store.store st t.block.family t.typ.write value) (function
          | Result.Error _ as result -> Lwt.return result
          | Result.Ok addr ->
            t.block.address <- Some addr;
            pump t.next
        )
  in
  Lwt.bind (pump queue) (function
      | Result.Error _ as result -> Lwt.return result
      | Result.Ok () ->
        let value = match block.value with
          | Block.In_store _ -> assert false
          | Block.In_memory v -> v
        in
        Store.store st block.family typ.write value
    )

let load st family address typ =
  Store.load st family address typ.read
