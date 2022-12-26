module type Field = sig
  type t

  val t : t Caqti_type.t
end

type ty =
  [ `Bool
  | `Int
  | `Int16
  | `Int32
  | `Int64
  | `Float
  | `String
  | `Octets
  | `Pdate
  | `Ptime
  | `Ptime_span
  | `Enum
  ]

module type Model = sig
  type t

  val t : t Caqti_type.t

  val name : string

  val fields : ty list
end
