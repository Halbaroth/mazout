type role =
  | Guest
  | Moderator
  | Admin
[@@deriving show] [@@deriving_inline queryable]

let _ = fun (_ : role) -> ()

let role_encode = function
  | Guest -> "Guest"
  | Moderator -> "Moderator"
  | Admin -> "Admin"

let _ = role_encode

let role_decode = function
  | "Guest" -> Ok Guest
  | "Moderator" -> Ok Moderator
  | "Admin" -> Ok Admin
  | _ as t -> Error (Format.sprintf "unknown case %s" t)

let _ = role_decode

let role = Caqti_type.enum ~encode:role_encode ~decode:role_decode "role"

let _ = role

[@@@end]

type status =
  [ `Employee
  | `Director
  ]
[@@deriving show] [@@deriving_inline queryable]

let _ = fun (_ : status) -> ()

let status_encode = function
  | `Employee -> "Employee"
  | `Director -> "Director"

let _ = status_encode

let status_decode = function
  | "Employee" -> Ok `Employee
  | "Director" -> Ok `Director
  | _ as t -> Error (Format.sprintf "unknown case %s" t)

let _ = status_decode

let status =
  Caqti_type.enum ~encode:status_encode ~decode:status_decode "status"

let _ = status

[@@@end]

type user = {
  name : string; [@not_null]
  age : int; [@not_null]
  role : role; [@not_null] [@default Guest]
  status : status;
  id : int; [@primary_key]
}
[@@deriving make, show] [@@deriving_inline queryable]

let _ = fun (_ : user) -> ()

let user_encode { name; age; role; status; id } =
  Ok (name, (age, (role, (status, id))))
let _ = user_encode

let user_decode (name, (age, (role, (status, id)))) =
  Ok { name; age; role; status; id }

let _ = user_decode

let user =
  Caqti_type.custom ~encode:user_encode ~decode:user_decode
    (Caqti_type.tup2 Caqti_type.Std.string
       (Caqti_type.tup2 Caqti_type.Std.int
          (Caqti_type.tup2 converter_role
             (Caqti_type.tup2 converter_status Caqti_type.Std.int) ) ) )

let _ = user

[@@@end]
