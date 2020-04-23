type 'u t = 'u Value.store = {
  mem : 'k. ('k, 'u) Address.family -> 'k -> (bool, 'u) Address.ulwt;
  load : 'k 'a. ('k, 'u) Address.family -> 'k ->
    ('u Value.input -> 'a) -> ('a, 'u) Address.ulwt;
  store : 'k 'a. ('k, 'u) Address.family ->
    ('u Value.output -> 'a -> unit) -> 'a -> ('k, 'u) Address.ulwt;
}

val mem : 'u t -> ('k, 'u) Address.family -> 'k -> (bool, 'u) Address.ulwt

val load : 'u t -> ('k, 'u) Address.family -> 'k ->
  ('u Value.input -> 'a) -> ('a,'u) Address.ulwt

val store : 'u t -> ('k, 'u) Address.family ->
  ('u Value.output -> 'a -> unit) -> 'a -> ('k,'u) Address.ulwt
