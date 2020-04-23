type role = string

type 'u input_ref =
  { pop: 'k. ('k, 'u) Address.family -> role * 'k }
[@@ocaml.unboxed]

type 'u output_ref =
  { push: 'k. ('k, 'u) Address.family -> role -> 'k -> unit }
[@@ocaml.unboxed]

type 'u input = {
  mutable ibuf: bytes;
  mutable cursor: int;
  mutable limit: int;
  mutable input_ref: 'u input_ref;
  mutable input_store : 'u store;
}

and 'u output = {
  universe: 'u Address.universe;
  (* A buffer of at least 8 bytes to serve as a scratch pad for output
       functions *)
  obuf: bytes;
  mutable output_data: string -> offset:int -> length:int -> unit;
  mutable output_ref: 'u output_ref;
  mutable output_hash : 'u Address.hash_sink;
}

and 'u store = {
  mem : 'k. ('k, 'u) Address.family -> 'k -> (bool, 'u) Address.ulwt;
  load : 'k 'a. ('k, 'u) Address.family -> 'k ->
    ('u input -> 'a) -> ('a, 'u) Address.ulwt;
  store : 'k 'a. ('k, 'u) Address.family ->
    ('u output -> 'a -> unit) -> 'a -> ('k, 'u) Address.ulwt;
}

let output_buf out length =
  out.output_data (Bytes.unsafe_to_string out.obuf) ~offset:0 ~length;
  out.universe.hash_data out.output_hash
    (Bytes.unsafe_to_string out.obuf) 0 length

let output_int8      out n = Bytes.set_int8      out.obuf 0 n; output_buf out 1
let output_uint8     out n = Bytes.set_uint8     out.obuf 0 n; output_buf out 1
let output_int16_ne  out n = Bytes.set_int16_ne  out.obuf 0 n; output_buf out 2
let output_int16_le  out n = Bytes.set_int16_le  out.obuf 0 n; output_buf out 2
let output_int16_be  out n = Bytes.set_int16_be  out.obuf 0 n; output_buf out 2
let output_uint16_ne out n = Bytes.set_uint16_ne out.obuf 0 n; output_buf out 2
let output_uint16_le out n = Bytes.set_uint16_le out.obuf 0 n; output_buf out 2
let output_uint16_be out n = Bytes.set_uint16_be out.obuf 0 n; output_buf out 2
let output_int32_ne  out n = Bytes.set_int32_ne  out.obuf 0 n; output_buf out 4
let output_int32_le  out n = Bytes.set_int32_le  out.obuf 0 n; output_buf out 4
let output_int32_be  out n = Bytes.set_int32_be  out.obuf 0 n; output_buf out 4
let output_int64_ne  out n = Bytes.set_int64_ne  out.obuf 0 n; output_buf out 8
let output_int64_le  out n = Bytes.set_int64_le  out.obuf 0 n; output_buf out 8
let output_int64_be  out n = Bytes.set_int64_be  out.obuf 0 n; output_buf out 8

let output_substring out str ~offset ~length =
  out.output_data str ~offset ~length;
  out.universe.hash_data out.output_hash
    (Bytes.unsafe_to_string out.obuf) 0 length

let output_ref out family role address =
  out.output_ref.push family role address;
  out.universe.hash_addr out.output_hash family role address

let invalid_store : 'u. 'u store =
  let fail _ = failwith "Value.input: uninitialized store" in
  { mem = fail; load = fail; store = fail }

let input_buf inp len =
  let offset = inp.cursor in
  let cursor = offset + len in
  if inp.limit < cursor then invalid_arg "Value.input: out of bounds";
  inp.cursor <- cursor;
  offset

let input_int8      inp = let i = input_buf inp 1 in Bytes.get_int8      inp.ibuf i
let input_uint8     inp = let i = input_buf inp 1 in Bytes.get_uint8     inp.ibuf i
let input_int16_ne  inp = let i = input_buf inp 2 in Bytes.get_int16_ne  inp.ibuf i
let input_int16_le  inp = let i = input_buf inp 2 in Bytes.get_int16_le  inp.ibuf i
let input_int16_be  inp = let i = input_buf inp 2 in Bytes.get_int16_be  inp.ibuf i
let input_uint16_ne inp = let i = input_buf inp 2 in Bytes.get_uint16_ne inp.ibuf i
let input_uint16_le inp = let i = input_buf inp 2 in Bytes.get_uint16_le inp.ibuf i
let input_uint16_be inp = let i = input_buf inp 2 in Bytes.get_uint16_be inp.ibuf i
let input_int32_ne  inp = let i = input_buf inp 4 in Bytes.get_int32_ne  inp.ibuf i
let input_int32_le  inp = let i = input_buf inp 4 in Bytes.get_int32_le  inp.ibuf i
let input_int32_be  inp = let i = input_buf inp 4 in Bytes.get_int32_be  inp.ibuf i
let input_int64_ne  inp = let i = input_buf inp 8 in Bytes.get_int64_ne  inp.ibuf i
let input_int64_le  inp = let i = input_buf inp 8 in Bytes.get_int64_le  inp.ibuf i
let input_int64_be  inp = let i = input_buf inp 8 in Bytes.get_int64_be  inp.ibuf i

let input_bytes inp buf ofs len =
  let offset = input_buf inp len in
  Bytes.blit inp.ibuf offset buf ofs len

let input_string inp len =
  let offset = input_buf inp len in
  Bytes.sub_string inp.ibuf offset len

let input_ref inp family = inp.input_ref.pop family

let input_remaining inp = inp.limit - inp.cursor

let fail_output _ = failwith "Value.output: output has not been setup"
let fail_input _ = failwith "Value.input: input has not been setup"

let make_output universe =
  let obuf = Bytes.make 8 '\x00' in
  { universe; obuf; output_data = fail_output;
    output_ref = { push = fail_output};
    output_hash = Address.null_sink }

let make_input () =
  { ibuf = Bytes.empty; limit = 0; cursor = 0;
    input_ref = { pop = fail_input }; input_store = invalid_store }

let flush_output out =
  out.output_data <- fail_output;
  out.output_ref <- { push = fail_output}

let flush_input inp =
  inp.ibuf <- Bytes.empty;
  inp.cursor <- 0;
  inp.limit <- 0;
  inp.input_ref <- { pop = fail_input };
  inp.input_store <- invalid_store

let setup_input inp ~buffer cursor len ~store ~ref =
  let limit = cursor + len in
  let length = Bytes.length buffer in
  if cursor < 0 || limit > length || limit < cursor then
    invalid_arg "Value.setup_input: window out of buffer bounds";
  inp.ibuf <- buffer;
  inp.cursor <- cursor;
  inp.limit <- limit;
  inp.input_ref <- ref;
  inp.input_store <- store

let input_store inp = inp.input_store

let setup_output out ~data ~ref ~hash =
  out.output_data <- data;
  out.output_ref <- ref;
  out.output_hash <- hash
