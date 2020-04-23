(* An address of type 'k pointing to a value of type 'a in universe 'u *)
type ('k, 'a, 'u) t = {
  family: ('k, 'u) Address.family;
  mutable value: ('a, 'u) value;
  mutable address: 'k option;
}

and ('a, 'u) value =
  | In_memory of 'a
  | In_store of 'u Store.t * ('u Value.input -> 'a)

let in_memory family v  = { family; address = None; value = In_memory v }

let in_store family store address loader =
  { family; address = Some address; value = In_store (store, loader) }

let dereference t =
  match t.value with
  | In_memory v -> Lwt.return_ok v
  | In_store (store, loader) ->
    match t.address with
    | None -> assert false
    | Some k -> store.load t.family k loader
