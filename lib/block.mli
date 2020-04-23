(* An address of type 'k pointing to a value of type 'a in universe 'u *)
type ('k, 'a, 'u) t = {
  family: ('k, 'u) Address.family;
  mutable value: ('a, 'u) value;
  mutable address: 'k option;
}

and ('a, 'u) value =
  | In_memory of 'a
  | In_store of 'u Store.t * ('u Value.input -> 'a)

val in_memory: ('k, 'u) Address.family -> 'a -> ('k, 'a, 'u) t
val in_store: ('k, 'u) Address.family -> 'u Store.t ->
  'k -> ('u Value.input -> 'a) -> ('k, 'a,' u) t

val dereference: ('k, 'a, 'u) t -> ('a, 'u) Address.ulwt
