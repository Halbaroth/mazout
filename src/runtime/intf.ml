type 'a cstr =
  | Not_null
  | Unique
  | Primary_key
  | Foreign_key of { name : string; key: int }
  | Default of 'a

type wit = private
  | Bool of bool cstr list
  | Int of int cstr list
  | Int16 of int cstr list
  | Int32 of int32 cstr list
  | Int64 of int64 cstr list
  | Float of float cstr list
  | String of string cstr list
  | Octets of string cstr list
  | Pdate of Ptime.t cstr list
  | Ptime of Ptime.t cstr list
  | Ptime_span of Ptime.Span.t cstr list
  | Enum of string cstr list

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

module type Model1 = sig
  include Model

  val primary_key : t -> int
end
