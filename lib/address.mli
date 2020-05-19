type ('a, 'b) ordering = Lt | Eq : ('a, 'a) ordering | Gt

type role = string

module type DESCRIPTION = sig
  type 'a family
  type error
  val order : 'a family -> 'b family -> ('a, 'b) ordering
end

type 'u error
type ('u, 't) family

type ('a, 'u) ulwt = ('a, 'u error) result Lwt.t

type 'u universe = private {
  order: 'a 'b. ('a, 'u) family -> ('b, 'u) family -> ('a, 'b) ordering;
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
end

module Make (D : DESCRIPTION) :
  SPACE with type 'a _family := 'a D.family
         and type _error := D.error
