open Ppxlib

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

type cstr =
  [ `Not_null
  | `Unique
  | `Primary_key
  | `Foreign_key
  | `Default
  ]

type meta = wit * cstr list

val queryable : Deriving.t
