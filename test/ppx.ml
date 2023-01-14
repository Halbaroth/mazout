type role =
  | Guest
  | Moderator
  | Admin
[@@deriving show, queryable]

type user = {
  name : string; [@not_null]
  age : int; [@not_null]
  role : role;
  id : int; [@primary_key]
}
[@@deriving make, show, queryable]

let pierre = make_user ~name:"Pierre" ~age:32 ~role:Moderator ~id:0

let user_id (user : user) = user.id

type post = {
  title : string; [@not_null]
  content : string;
  author_id : user; [@default pierre]
  id : int; [@primary_key]
}
[@@deriving make, show, queryable]

type status = Draft | Published [@@deriving show, queryable]

module Commentary = struct
  type t = {
    author : user; [@not_null]
    post : post; [@not_null]
    content : string;
    status : status;
    id : int; [@primary_key]
  } [@@deriving make, show, queryable]
end
