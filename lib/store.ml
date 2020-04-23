type 'u t = 'u Value.store = {
  mem : 'k. ('k, 'u) Address.family -> 'k -> (bool, 'u) Address.ulwt;
  load : 'k 'a. ('k, 'u) Address.family -> 'k ->
    ('u Value.input -> 'a) -> ('a, 'u) Address.ulwt;
  store : 'k 'a. ('k, 'u) Address.family ->
    ('u Value.output -> 'a -> unit) -> 'a -> ('k, 'u) Address.ulwt;
}
let mem t a = t.mem a
let load t a f = t.load a f
let store t a f = t.store a f
