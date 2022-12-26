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

type model

and cstr =
  [ `Not_null
  | `Unique
  | `Primary_key
  | `Foreign_key of model
  | `Default of model
  ]

module type Field = sig
  type t

  val t : t Caqti_type.t
end

module type Model = sig
  type t

  val t : t Caqti_type.t

  val name : string

  val fields : (string * wit * cstr list) list
end

module type Model1 = sig
  include Model

  val primary_key : t -> int
end

val wit : (module Model) -> model
