type role = string

type 'u input
type 'u output

type 'u store = {
  mem : 'k. ('k, 'u) Address.family -> 'k -> (bool, 'u) Address.ulwt;
  load : 'k 'a. ('k, 'u) Address.family -> 'k ->
    ('u input -> 'a) -> ('a, 'u) Address.ulwt;
  store : 'k 'a. ('k, 'u) Address.family ->
    ('u output -> 'a -> unit) -> 'a -> ('k, 'u) Address.ulwt;
}


(* Output *)

val output_int8      : _ output -> int -> unit
val output_uint8     : _ output -> int -> unit
val output_int16_ne  : _ output -> int -> unit
val output_int16_le  : _ output -> int -> unit
val output_int16_be  : _ output -> int -> unit
val output_uint16_ne : _ output -> int -> unit
val output_uint16_le : _ output -> int -> unit
val output_uint16_be : _ output -> int -> unit
val output_int32_ne  : _ output -> int32 -> unit
val output_int32_le  : _ output -> int32 -> unit
val output_int32_be  : _ output -> int32 -> unit
val output_int64_ne  : _ output -> int64 -> unit
val output_int64_le  : _ output -> int64 -> unit
val output_int64_be  : _ output -> int64 -> unit

val output_substring : _ output -> string -> offset:int -> length:int -> unit
val output_ref : 'u output -> ('k, 'u) Address.family -> role -> 'k -> unit

val make_output : 'u Address.universe -> 'u output

type 'u output_ref =
  { push: 'k. ('k, 'u) Address.family -> role -> 'k -> unit }
[@@ocaml.unboxed]

val setup_output : 'u output ->
  data:(string -> offset:int -> length:int -> unit) ->
  ref:'u output_ref ->
  unit
val flush_output : _ output -> unit

(* Input *)

val input_remaining : _ input -> int
val input_int8      : _ input -> int
val input_uint8     : _ input -> int
val input_int16_ne  : _ input -> int
val input_int16_le  : _ input -> int
val input_int16_be  : _ input -> int
val input_uint16_ne : _ input -> int
val input_uint16_le : _ input -> int
val input_uint16_be : _ input -> int
val input_int32_ne  : _ input -> int32
val input_int32_le  : _ input -> int32
val input_int32_be  : _ input -> int32
val input_int64_ne  : _ input -> int64
val input_int64_le  : _ input -> int64
val input_int64_be  : _ input -> int64
val input_string : _ input -> int -> string
val input_bytes : _ input -> bytes -> int -> int -> unit
val input_store : 'u input -> 'u store
val input_ref : 'u input -> ('k, 'u) Address.family -> role * 'k

val make_input : unit -> 'u input

type 'u input_ref =
  { pop: 'k. ('k, 'u) Address.family -> role * 'k }
[@@ocaml.unboxed]

val setup_input : 'u input ->
  buffer:bytes -> int -> int -> store:'u store -> ref:'u input_ref -> unit

val flush_input : 'u input -> unit
