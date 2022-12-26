type wit =
  [ `Bool
  | `Int
  | `Int32
  | `Int64
  | `Float
  | `String
  | `Octets
  | `Ptime
  | `Ptime_span
  ]

module type M = sig
  type t

  type meta

  val t : t Caqti_type.t

  val name : string

  val fields : meta list
end

module type M1 = sig
  include M

  val primary_key : t -> int
end

type model = (module M1 with type meta = string * wit * cstr)

and cstr =
  [ `Not_null
  | `Unique
  | `Primary_key
  | `Foreign_key of (module M)
  | `Default of (module M)
  ]

module type Field = sig
  type t

  val t : t Caqti_type.t
end

module type Model = sig
  include M with type meta = string * wit * cstr
end

module type Model1 = sig
  include M1 with type meta = string * wit * cstr
end
