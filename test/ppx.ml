type role =
  | Guest
  | Moderator
  | Admin
[@@deriving show] [@@deriving_inline queryable]

let _ = fun (_ : role) -> ()

let converter_role =
  let decode = function
    | "Guest" -> Ok Guest
    | "Moderator" -> Ok Moderator
    | "Admin" -> Ok Admin
    | _ as t -> Error (Format.sprintf "unknown case %s" t)
  in
  let encode = function
    | Guest -> "Guest"
    | Moderator -> "Moderator"
    | Admin -> "Admin"
  in
  Caqti_type.enum ~encode ~decode "role"

let _ = converter_role

[@@@end]

type status =
  [ `Employee
  | `Director
  ]
[@@deriving show] [@@deriving_inline queryable]

let _ = fun (_ : status) -> ()

let converter_status =
  let decode = function
    | "Employee" -> Ok `Employee
    | "Director" -> Ok `Director
    | _ as t -> Error (Format.sprintf "unknown case %s" t)
  in
  let encode = function
    | `Employee -> "Employee"
    | `Director -> "Director"
  in
  Caqti_type.enum ~encode ~decode "status"

let _ = converter_status

[@@@end]

type user = {
  name : string;
  role : role;
  status : status;
  id : int;
}
[@@deriving make, show] [@@deriving_inline queryable]

let _ = fun (_ : user) -> ()

let converter_user =
  let decode (name, (role, (status, id))) = Ok { name; role; status; id } in
  let encode { name; role; status; id } = Ok (name, (role, (status, id))) in
  Caqti_type.custom ~encode ~decode
    (Caqti_type.tup2 Caqti_type.Std.string
       (Caqti_type.tup2 converter_role
          (Caqti_type.tup2 converter_status Caqti_type.Std.int) ) )

let _ = converter_user

let fields_user = [ "name"; "role"; "status"; "id" ]

let _ = fields_user

[@@@end]
