open Ppxlib
module List = ListLabels
module Builder = Ast_builder.Default

let lident_of_string ~loc str =
  Loc.make ~loc (Astlib.Longident.parse str) |> Builder.pexp_ident ~loc

let exp_of_label_decl { pld_name = { loc; txt }; _ } = lident_of_string ~loc txt

let pat_of_label_decl { pld_name = { loc; _ } as name; _ } =
  Builder.ppat_var ~loc name

(* Produce a expression {field1; field2; ...} from the label declarations
   [field1; field2; ...] *)
let record_exp_of_label_decl ~loc lds =
  let exps =
    List.map lds ~f:(fun { pld_name = { loc; txt }; _ } ->
        let lident = Loc.make ~loc (Lident txt) in
        (lident, Builder.pexp_ident ~loc lident) )
  in
  Builder.pexp_record ~loc exps None

(* Produce a pattern {field1; field2; ...} from the label declarations
   [field1; field2; ...] *)
let record_pat_of_label_decl ~loc lds =
  let pats =
    List.map lds ~f:(fun { pld_name = { loc; txt } as name; _ } ->
        let lident = Loc.make ~loc (Lident txt) in
        (lident, Builder.ppat_var ~loc name) )
  in
  Builder.ppat_record ~loc pats Closed

(* Generate a pattern (x, (y, ...)) from the list [x; y; ...]. *)
let rec nested_tuple_pat ~loc = function
  | [] -> [%pat? ()]
  | [ pat ] -> pat
  | hd :: [ pat ] -> [%pat? [%p hd], [%p pat]]
  | hd :: tl ->
    let pat = nested_tuple_pat ~loc tl in
    [%pat? [%p hd], [%p pat]]

(* Generate an expression (x, (y, ...)) from the list [x; y; ...]. *)
let rec nested_tuple_expr ~loc = function
  | [] -> [%expr ()]
  | [ expr ] -> expr
  | hd :: [ expr ] -> [%expr [%e hd], [%e expr]]
  | hd :: tl ->
    let expr = nested_tuple_expr ~loc tl in
    [%expr [%e hd], [%e expr]]

(* Generate an expression Caqti_type.tup2 (x, Caqti_type.tup2 (y, ...)) from
   the list [x; y; ...]. *)
let rec caqti_wit_tuple ~loc = function
  | [] -> [%expr unit]
  | [ expr ] -> expr
  | hd :: [ expr ] -> [%expr Caqti_type.tup2 [%e hd] [%e expr]]
  | hd :: tl ->
    let expr = caqti_wit_tuple ~loc tl in
    [%expr Caqti_type.tup2 [%e hd] [%e expr]]

(* Generate an expression x * (y * ...) from the list of types [x; y; ...]. *)
(*let rec type_tuple ~loc = function
  | [] -> [%type: unit]
  | [ty] -> ty
  | hd :: [ty] -> [%type: [%t hd] * [%t ty]]
  | hd :: tl ->
      let ty = type_tuple ~loc tl in
      [%type: [%t hd] * [%t ty]]*)

(* Generate the list of the labels of a record. *)
let label_list ~loc lds =
  List.map lds ~f:(fun { pld_name = { txt = label; _ }; _ } ->
      [%expr [%e Builder.estring ~loc label]] )
  |> Builder.elist ~loc

(* --------------------------------------- *)

let prefix ~(kind : [ `Converter | `Fields ]) name =
  match (kind, Base.String.rsplit2 name ~on:'.') with
  | `Converter, Some (_, "t") -> name
  | `Converter, Some (prefix, ty) -> prefix ^ ".converter_" ^ ty
  | `Converter, None -> begin
    match name with
    | "t" -> name
    | ty -> "converter_" ^ ty
  end
  | `Fields, Some (prefix, "t") -> prefix ^ ".fields"
  | `Fields, Some (prefix, ty) -> prefix ^ ".fields_" ^ ty
  | `Fields, None -> begin
    match name with
    | "t" -> "fields"
    | ty -> "fields_" ^ ty
  end

(* Produce the Caqti witness from a core type. *)
let rec caqti_wit_of_ty ~loc = function
  | [%type: bool] -> [%expr Caqti_type.Std.bool]
  | [%type: int] -> [%expr Caqti_type.Std.int]
  | [%type: int32] -> [%expr Caqti_type.Std.int32]
  | [%type: int64] -> [%expr Caqti_type.Std.int64]
  | [%type: float] -> [%expr Caqti_type.Std.float]
  | [%type: string] -> [%expr Caqti_type.Std.string]
  | [%type: Ptime.t t] -> [%expr Caqti_type.Std.ptime]
  | [%type: Ptime.span t] -> [%expr Caqti_type.Std.ptime_span]
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "option"; _ }, [ arg ]); _ } ->
    [%expr Caqti_type.option [%e caqti_wit_of_ty ~loc arg]]
  | { ptyp_desc = Ptyp_tuple tys; _ } -> begin
    match tys with
    | []
    | [ _ ] ->
      failwith "Ocaml invariant"
    | [ ty1; ty2 ] ->
      let ty1 = caqti_wit_of_ty ~loc ty1 in
      let ty2 = caqti_wit_of_ty ~loc ty2 in
      [%expr Caqti_type.tup2 [%e ty1] [%e ty2]]
    | [ ty1; ty2; ty3 ] ->
      let ty1 = caqti_wit_of_ty ~loc ty1 in
      let ty2 = caqti_wit_of_ty ~loc ty2 in
      let ty3 = caqti_wit_of_ty ~loc ty3 in
      [%expr Caqti_type.tup3 [%e ty1] [%e ty2] [%e ty3]]
    | [ ty1; ty2; ty3; ty4 ] ->
      let ty1 = caqti_wit_of_ty ~loc ty1 in
      let ty2 = caqti_wit_of_ty ~loc ty2 in
      let ty3 = caqti_wit_of_ty ~loc ty3 in
      let ty4 = caqti_wit_of_ty ~loc ty4 in
      [%expr Caqti_type.tup4 [%e ty1] [%e ty2] [%e ty3] [%e ty4]]
    | _ -> Location.raise_errorf ~loc "ppx_deriving_mazout: unsupported type"
  end
  | _ as ty ->
    let label = Ppxlib.string_of_core_type ty |> prefix ~kind:`Converter in
    lident_of_string ~loc label

let _mazout_wit_of_ty ~loc = function
  | [%type: bool] -> [%expr `Bool]
  | [%type: int] -> [%expr `Int]
  | [%type: int32] -> [%expr `Int32]
  | [%type: int64] -> [%expr `Int64]
  | [%type: float] -> [%expr `Float]
  | [%type: string] -> [%expr `String]
  | [%type: octets] -> [%expr `Octets]
  | [%type: Ptime.t t] -> [%expr `Ptime]
  | [%type: Ptime.span t] -> [%expr `Ptime_span]
  | _ -> [%expr `Int]

(* Produce the Caqti witness from the label declarations of a record. *)
let caqti_wit_of_record ~loc lds =
  let tys = List.map lds ~f:(fun { pld_type; _ } -> pld_type) in
  List.map tys ~f:(caqti_wit_of_ty ~loc) |> caqti_wit_tuple ~loc

(* Generate the body of the Caqti convertor for records. *)
let converter_body_of_record ~loc ~caqti_wit lds =
  let exps = List.map lds ~f:exp_of_label_decl in
  let pats = List.map lds ~f:pat_of_label_decl in
  [%expr
    let decode [%p nested_tuple_pat ~loc pats] =
      Ok [%e record_exp_of_label_decl ~loc lds]
    in
    let encode [%p record_pat_of_label_decl ~loc lds] =
      Ok [%e nested_tuple_expr ~loc exps]
    in
    Caqti_type.custom ~encode ~decode [%e caqti_wit]]

(* Generate the pattern _ as v *)
let ppat_wildcase ~loc ~alias =
  let any = Builder.ppat_any ~loc in
  match alias with
  | None -> any
  | Some v -> Builder.(ppat_alias ~loc any (Located.mk ~loc v))

(* Generate the body of the Caqti convertor for enumerations. *)
let converter_body_of_enum ~loc ~name cstrs =
  let error_case =
    let rhs =
      [%expr
        Error (Format.sprintf "unknown case %s" [%e Builder.evar ~loc "t"])]
    in
    Builder.case ~lhs:(ppat_wildcase ~loc ~alias:(Some "t")) ~guard:None ~rhs
  in
  let decode_cases =
    List.map cstrs ~f:(fun { pcd_name = { txt; _ }; _ } ->
        let lhs = Builder.pstring ~loc txt in
        let lident = Loc.make ~loc (Astlib.Longident.parse txt) in
        let rhs = [%expr Ok [%e Builder.pexp_construct ~loc lident None]] in
        Builder.case ~lhs ~guard:None ~rhs )
    @ [ error_case ]
  in
  let encode_cases =
    List.map cstrs ~f:(fun { pcd_name = { txt; _ }; _ } ->
        let lident = Loc.make ~loc (Astlib.Longident.parse txt) in
        let lhs = Builder.ppat_construct ~loc lident None in
        let rhs = Builder.estring ~loc txt in
        Builder.case ~lhs ~guard:None ~rhs )
  in
  [%expr
    let decode = [%e Builder.pexp_function ~loc decode_cases] in
    let encode = [%e Builder.pexp_function ~loc encode_cases] in
    Caqti_type.enum ~encode ~decode [%e Builder.estring ~loc name]]

(* Test if all the constructor of a polymorphic variant type have no
   payload. *)
let converter_body_of_polymorphic_enum ~loc ~name row_fields =
  let error_case =
    let rhs =
      [%expr
        Error (Format.sprintf "unknown case %s" [%e Builder.evar ~loc "t"])]
    in
    Builder.case ~lhs:(ppat_wildcase ~loc ~alias:(Some "t")) ~guard:None ~rhs
  in
  let decode_cases =
    List.map row_fields ~f:(fun { prf_desc; _ } ->
        match prf_desc with
        | Rtag ({ txt; _ }, _, _) ->
          let lhs = Builder.pstring ~loc txt in
          let rhs = [%expr Ok [%e Builder.pexp_variant ~loc txt None]] in
          Builder.case ~lhs ~guard:None ~rhs
        | Rinherit { ptyp_desc = Ptyp_variant (_row_fields, _, _); _ } ->
          Location.raise_errorf ~loc
            "ppx_deriving_mazout: extension not yet supported"
        | _ ->
          Location.raise_errorf ~loc "ppx_deriving_mazout: malformed variant" )
    @ [ error_case ]
  in
  let encode_cases =
    List.map row_fields ~f:(fun { prf_desc; _ } ->
        match prf_desc with
        | Rtag ({ txt; _ }, _, _) ->
          let lhs = Builder.ppat_variant ~loc txt None in
          let rhs = Builder.estring ~loc txt in
          Builder.case ~lhs ~guard:None ~rhs
        | Rinherit { ptyp_desc = Ptyp_variant (_row_fields, _, _); _ } ->
          Location.raise_errorf ~loc
            "ppx_deriving_mazout: extension not yet supported"
        | _ ->
          Location.raise_errorf ~loc "ppx_deriving_mazout: malformed variant" )
  in
  [%expr
    let decode = [%e Builder.pexp_function ~loc decode_cases] in
    let encode = [%e Builder.pexp_function ~loc encode_cases] in
    Caqti_type.enum ~encode ~decode [%e Builder.estring ~loc name]]

let fields_list_of_record ~loc ~name lds =
  let label = Builder.Located.mk ~loc (prefix ~kind:`Fields name) in
  [%stri let [%p Builder.ppat_var ~loc label] = [%e label_list ~loc lds]]

(* Test if all the constructors of a variant type have no payload. *)
let is_enum cstr_decls =
  List.for_all cstr_decls ~f:(fun { pcd_res; pcd_args; _ } ->
      pcd_res = None
      &&
      match pcd_args with
      | Pcstr_tuple [] -> true
      | Pcstr_record _
      | Pcstr_tuple _ ->
        false )

(* Test if all the constructor of a polymorphic variant type have no
   payload. *)
let rec is_polymorphic_enum row_fields =
  List.for_all row_fields ~f:(fun { prf_desc; _ } ->
      match prf_desc with
      | Rtag (_, true, []) -> true
      | Rinherit { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
        is_polymorphic_enum row_fields
      | Rtag _
      | Rinherit _ ->
        false )

let str_of_type_decl
    {
      ptype_name = { txt = name; _ };
      ptype_loc = loc;
      ptype_kind;
      ptype_manifest;
      _;
    } =
  let label = Builder.Located.mk ~loc (prefix ~kind:`Converter name) in
  match ptype_kind with
  | Ptype_record lds ->
    let caqti_wit = caqti_wit_of_record ~loc lds in
    let body = converter_body_of_record ~loc ~caqti_wit lds in
    [
      [%stri let [%p Builder.ppat_var ~loc label] = [%e body]];
      fields_list_of_record ~loc ~name lds;
    ]
  | Ptype_abstract -> begin
    match ptype_manifest with
    | Some { ptyp_desc = Ptyp_variant (row_fields, _, _); _ }
      when is_polymorphic_enum row_fields ->
      let body = converter_body_of_polymorphic_enum ~loc ~name row_fields in
      [%str let [%p Builder.ppat_var ~loc label] = [%e body]]
    | Some ty -> begin
      let caqti_wit = caqti_wit_of_ty ~loc ty in
      [%str let [%p Builder.ppat_var ~loc label] = [%e caqti_wit]]
    end
    | None -> Location.raise_errorf ~loc "ppx_deriving_mazout: unsupported type"
  end
  | Ptype_variant cstr_decls when is_enum cstr_decls ->
    let body = converter_body_of_enum ~loc ~name cstr_decls in
    [%str let [%p Builder.ppat_var ~loc label] = [%e body]]
  | Ptype_open
  | Ptype_variant _ ->
    Location.raise_errorf ~loc "ppx_deriving_mazout: unsupported type"

let str_of_type_decls ~ctxt (_rec_flag, tds) =
  let _loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map tds ~f:str_of_type_decl |> List.concat

let sig_value ~loc ~txt ~type_ =
  Builder.(
    let name = { loc; txt } in
    value_description ~loc ~name ~type_ ~prim:[] |> psig_value ~loc )

let sig_converter_of_type_decl
    ({ ptype_name = { txt = name; _ }; ptype_loc = loc; _ } as td) =
  let ty = core_type_of_type_declaration td in
  let type_ = [%type: [%t ty] Caqti_type.t] in
  [ sig_value ~loc ~txt:name ~type_ ]

let sig_of_type_decls ~ctxt (_rec_flag, tds) =
  let _loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map tds ~f:sig_converter_of_type_decl |> List.concat

let queryable =
  let str_type_decl = Deriving.Generator.V2.make_noarg str_of_type_decls in
  let sig_type_decl = Deriving.Generator.V2.make_noarg sig_of_type_decls in
  Deriving.add "queryable" ~str_type_decl ~sig_type_decl
