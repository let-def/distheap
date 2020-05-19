type ('a, 'b) ordering = Lt | Eq : ('a, 'a) ordering | Gt

type role = string

module type DESCRIPTION = sig
  type 'a family
  type error
  val order : 'a family -> 'b family -> ('a, 'b) ordering

  type 'a hasher
  val hash_make  : 'a family -> 'a hasher
  val hash_reset : 'a hasher -> 'a family -> unit
  val hash_data  : 'a hasher -> string -> int -> int -> unit
  val hash_addr  : 'a hasher -> 'b family -> role -> 'b -> unit
  val hash_flush : 'a hasher -> 'a
end

type 'u error
type ('a, 'u) family

type ('a, 'u) ulwt = ('a, 'u error) result Lwt.t

type ('a, 'u) hasher
type 'u hash_sink = H : ('a, 'u) hasher -> 'u hash_sink | N
let hash_sink h = H h
let null_sink = N

type 'u universe = {
  order: 'a 'b. ('a, 'u) family -> ('b, 'u) family -> ('a, 'b) ordering;
  hash_make  : 'a. ('a, 'u) family -> ('a, 'u) hasher;
  hash_reset : 'a. ('a, 'u) hasher -> ('a, 'u) family -> unit;
  hash_data  : 'u hash_sink -> string -> int -> int -> unit;
  hash_addr  : 'b. 'u hash_sink -> ('b, 'u) family -> role -> 'b -> unit;
  hash_flush : 'a. ('a, 'u) hasher -> 'a;
}

module type SPACE = sig
  type u
  val u : u universe

  type 'a _family
  external inj_family : 'a _family -> ('a, u) family = "%identity"
  external prj_family : ('a, u) family -> 'a _family = "%identity"

  type _error
  external inj_error : _error -> u error = "%identity"
  external prj_error : u error -> _error = "%identity"

  type 'a _hasher
  external inj_hasher : 'a _hasher -> ('a, u) hasher = "%identity"
  external prj_hasher : ('a, u) hasher -> 'a _hasher = "%identity"
end

module Make (D : DESCRIPTION) :
  SPACE with type 'a _family := 'a D.family
         and type _error := D.error
         and type 'a _hasher := 'a D.hasher
= struct
  type u

  external inj_family : 'a D.family -> ('a, u) family = "%identity"
  external prj_family : ('a, u) family -> 'a D.family = "%identity"

  external inj_error : D.error -> u error = "%identity"
  external prj_error : u error -> D.error = "%identity"

  external inj_hasher : 'a D.hasher -> ('a, u) hasher = "%identity"
  external prj_hasher : ('a, u) hasher -> 'a D.hasher = "%identity"

  let u : u universe = {
    order = (fun k1 k2 -> D.order (prj_family k1) (prj_family k2));
    hash_make  = (fun f -> inj_hasher (D.hash_make (prj_family f)));
    hash_reset = (fun h f -> D.hash_reset (prj_hasher h) (prj_family f));
    hash_data  = (fun h s o l -> match h with
        | N -> ()
        | H h -> D.hash_data (prj_hasher h) s o l);
    hash_addr  = (fun h f role a -> match h with
        | N -> ()
        | H h -> D.hash_addr (prj_hasher h) (prj_family f) role a);
    hash_flush = (fun h -> D.hash_flush (prj_hasher h));
  }
end
