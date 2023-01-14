type 'a cstr =
  | Not_null
  | Unique
  | Primary_key
  | Foreign_key
  | Default of 'a

type 'a cstrs = 'a cstr list

type wit =
  | Bool of bool cstrs
  | Int of int cstrs
  | Int16 of int cstrs
  | Int32 of int32 cstrs
  | Int64 of int64 cstrs
  | Float of float cstrs
  | String of string cstrs
  | Octets of string cstrs
  | Pdate of Ptime.t cstrs
  | Ptime of Ptime.t cstrs
  | Ptime_span of Ptime.Span.t cstrs
  | Enum of string cstrs

module type Field = sig
  type t

  val t : t Caqti_type.t
end

module type Model = sig
  type t

  val t : t Caqti_type.t

  val name : string

  val fields : (string * wit) list
end

module type Model_with_key = sig
  include Model

  val primary_key : t -> int
end
