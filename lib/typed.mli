type ('a, 'u) typ = {
  read: 'u Value.input -> 'a;
  write: 'u Value.output -> 'a -> unit;
  traverse: 'acc. (('acc, 'u) traversal -> 'acc -> 'a -> 'acc) option;
}

and ('acc, 'u) traversal = {
  step: 'k 'a. 'acc -> ('a, 'u) typ -> ('k, 'a, 'u) Block.t -> 'acc;
} [@@ocaml.unboxed]

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

val unit : (unit, 'u) typ
val int32 : (int32, 'u) typ
val string : (string, 'u) typ
val option : ('a, 'u) typ -> ('a option, 'u) typ

val either : ('a, 'u) typ -> ('b, 'u) typ -> (('a, 'b) either, 'u) typ
val pair : ('a, 'u) typ -> ('b, 'u) typ -> ('a * 'b, 'u) typ

val bimap : ('a -> 'b) -> ('b -> 'a) -> ('a, 'u) typ -> ('b, 'u) typ

val addr : Value.role -> ('k, 'u) Address.family -> ('a, 'u) typ ->
  (('k, 'a, 'u) Block.t, 'u) typ

type ('a, 'u) recursive
val recursion : unit -> ('a, 'u) recursive
val recurse : ('a, 'u) recursive -> ('a, 'u) typ
val seal : ('a, 'u) recursive -> ('a, 'u) typ -> ('a, 'u) typ

(* Sample: indirect list *)
type ('k, 'a, 'u) ilist =
  | Nil
  | Cons of 'a * ('k, ('k, 'a, 'u) ilist, 'u) Block.t

val ilist : ('k, 'u) Address.family -> ('a, 'u) typ ->
  (('k, 'a, 'u) ilist, 'u) typ

val store : 'u Store.t ->
  ('a, 'u) typ -> ('k, 'a, 'u) Block.t -> ('k, 'u) Address.ulwt

val load : 'u Store.t ->
  ('k, 'u) Address.family -> 'k -> ('a, 'u) typ -> ('a, 'u) Address.ulwt
