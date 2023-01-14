open Ppxlib
module List = ListLabels
module Builder = Ast_builder.Default

(*let default =
  Attribute.declare "mazout.default" Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil))*)

(* --------------------------------------- *)

let lident_of_label { txt; loc } =
  Builder.(Astlib.Longident.parse txt |> Located.mk ~loc |> pexp_ident ~loc)

let exp_of_label_decl { pld_name; _ } = lident_of_label pld_name

let pat_of_label_decl { pld_name = { loc; _ } as name; _ } =
  Builder.ppat_var ~loc name

(* Produce a expression {field1; field2; ...} from the label declarations
   [field1; field2; ...]. *)
let record_exp_of_label_decl ~loc lds =
  let exps =
    List.map lds ~f:(fun { pld_name = { loc; txt }; _ } ->
        let lident = Loc.make ~loc (Lident txt) in
        (lident, Builder.pexp_ident ~loc lident) )
  in
  Builder.pexp_record ~loc exps None

(* Produce a pattern {field1; field2; ...} from the label declarations
   [field1; field2; ...]. *)
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
let caqti_wit_tuple =
  let rec aux ~loc = function
    | [] -> [%expr unit]
    | [ expr ] -> expr
    | hd :: [ expr ] -> [%expr tup2 [%e hd] [%e expr]]
    | hd :: tl ->
      let expr = aux ~loc tl in
      [%expr tup2 [%e hd] [%e expr]]
  in
  fun ~loc lst -> [%expr Caqti_type.([%e aux ~loc lst])]

(* Generate the type x * (y * ...) from the list of types [x; y; ...]. *)
(*let rec type_tuple ~loc = function
  | [] -> [%type: unit]
  | [ty] -> ty
  | hd :: [ty] -> [%type: [%t hd] * [%t ty]]
  | hd :: tl ->
      let ty = type_tuple ~loc tl in
      [%type: [%t hd] * [%t ty]]*)

(* Generate the pattern _ as v. *)
let ppat_wildcase ~loc ~alias =
  let any = Builder.ppat_any ~loc in
  match alias with
  | None -> any
  | Some v -> Builder.(ppat_alias ~loc any (Located.mk ~loc v))

(* --------------------------------------- *)

let prefix ~(kind : [ `Converter | `Fields ]) name =
  match (kind, Base.String.rsplit2 name ~on:'.') with
  | `Converter, Some (_, "t") -> name
  | `Converter, Some (prefix, ty) -> prefix ^ ".converter_" ^ ty
  | `Converter, None -> begin
    match name with
    | "t" -> name
    | ty -> ty
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
  | [%type: bool] -> [%expr Std.bool]
  | [%type: int] -> [%expr Std.int]
  | [%type: int32] -> [%expr Std.int32]
  | [%type: int64] -> [%expr Std.int64]
  | [%type: float] -> [%expr Std.float]
  | [%type: string] -> [%expr Std.string]
  | [%type: Ptime.t t] -> [%expr Std.ptime]
  | [%type: Ptime.span t] -> [%expr Std.ptime_span]
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
      [%expr tup2 [%e ty1] [%e ty2]]
    | [ ty1; ty2; ty3 ] ->
      let ty1 = caqti_wit_of_ty ~loc ty1 in
      let ty2 = caqti_wit_of_ty ~loc ty2 in
      let ty3 = caqti_wit_of_ty ~loc ty3 in
      [%expr tup3 [%e ty1] [%e ty2] [%e ty3]]
    | [ ty1; ty2; ty3; ty4 ] ->
      let ty1 = caqti_wit_of_ty ~loc ty1 in
      let ty2 = caqti_wit_of_ty ~loc ty2 in
      let ty3 = caqti_wit_of_ty ~loc ty3 in
      let ty4 = caqti_wit_of_ty ~loc ty4 in
      [%expr tup4 [%e ty1] [%e ty2] [%e ty3] [%e ty4]]
    | _ -> Location.raise_errorf ~loc "ppx_deriving_mazout: unsupported type"
  end
  | _ as ty ->
    let label = Ppxlib.string_of_core_type ty |> prefix ~kind:`Converter in
    Builder.Located.mk ~loc label |> lident_of_label

(* TODO: Raise an exception if the argument is not a structure. *)
let constraint_of_attr ~loc ~converter
    { attr_name = { txt; _ }; attr_payload; _ } =
  match (txt, attr_payload) with
  | "not_null", PStr [] -> Some [%expr Not_null]
  | "unique", PStr [] -> Some [%expr Unique]
  | "primary_key", PStr [] -> Some [%expr Primary_key]
  | "foreign_key", PStr [] -> Some [%expr Foreign_key]
  | "default", PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> (
    match converter with
    | Some c ->
      let e = Builder.pexp_apply ~loc c [ (Nolabel, e) ] in
      Some [%expr Default [%e e]]
    | None -> Some [%expr Default [%e e]] )
  | _ -> None

let suffix_lident ~suffix = function
  | Lident s -> Lident (s ^ suffix)
  | Ldot (lident, s) -> Ldot (lident, s ^ suffix)
  | Lapply _ -> failwith "Invalid argument"

(* Produce the the list of mazout witnesses with sql attributes of
   a label declaration. *)
let mazout_wit_of_ty ~loc
    { pld_name = { txt = label; _ }; pld_type; pld_attributes; _ } =
  let cstrs ~converter =
    List.partition_map pld_attributes ~f:(fun attr ->
        match constraint_of_attr ~loc ~converter attr with
        | Some attr -> Left attr
        | None -> Right attr )
    |> fst |> Builder.elist ~loc
  in
  match pld_type with
  | [%type: bool] -> [%expr Bool [%e cstrs ~converter:None]]
  | [%type: int] -> [%expr Int [%e cstrs ~converter:None]]
  | [%type: int16] -> [%expr Int [%e cstrs ~converter:None]]
  | [%type: int32] -> [%expr Int32 [%e cstrs ~converter:None]]
  | [%type: int64] -> [%expr Int64 [%e cstrs ~converter:None]]
  | [%type: float] -> [%expr Float [%e cstrs ~converter:None]]
  | [%type: string] -> [%expr String [%e cstrs ~converter:None]]
  | [%type: octets] -> [%expr Octets [%e cstrs ~converter:None]]
  | [%type: Ptime.t t] -> [%expr Ptime [%e cstrs ~converter:None]]
  | [%type: Ptime.span t] -> [%expr Ptime_span [%e cstrs ~converter:None]]
  | { ptyp_desc = Ptyp_constr ({txt = lident; loc}, []); _ } ->
    let converter =
      suffix_lident ~suffix:"_id" lident
      |> Builder.Located.mk ~loc |> Builder.pexp_ident ~loc |> Option.some
    in
    [%expr Int [%e cstrs ~converter]]
  | _ ->
    let converter =
      Builder.Located.mk ~loc label |> lident_of_label |> Option.some
    in
    [%expr Int [%e cstrs ~converter]]

(* Generate the metainformations of labels of a record. *)
let label_list ~loc lds =
  List.map lds ~f:(fun ({ pld_name = { txt = label; _ }; _ } as ld) ->
      [%expr [%e Builder.estring ~loc label], [%e mazout_wit_of_ty ~loc ld]] )
  |> Builder.elist ~loc

(* Produce the Caqti witness from the label declarations of a record. *)
let caqti_wit_of_record ~loc lds =
  let tys = List.map lds ~f:(fun { pld_type; _ } -> pld_type) in
  List.map tys ~f:(caqti_wit_of_ty ~loc) |> caqti_wit_tuple ~loc

(* Generate the converter function for custom or enum fields. *)
let generate_converter ~loc ~name ~caqti_wit ~(kind : [ `Custom | `Enum ])
    ~encode ~decode =
  let encode_lb = Builder.Located.mk ~loc (name ^ "_encode") in
  let decode_lb = Builder.Located.mk ~loc (name ^ "_decode") in
  let wit_lb = Builder.Located.mk ~loc name in
  [%str
    let [%p Builder.ppat_var ~loc encode_lb] = [%e encode]

    let [%p Builder.ppat_var ~loc decode_lb] = [%e decode]]
  @
  match kind with
  | `Custom ->
    [%str
      let [%p Builder.ppat_var ~loc wit_lb] =
        Caqti_type.custom ~encode:[%e lident_of_label encode_lb]
          ~decode:[%e lident_of_label decode_lb] [%e caqti_wit]]
  | `Enum ->
    [%str
      let [%p Builder.ppat_var ~loc wit_lb] =
        Caqti_type.enum ~encode:[%e lident_of_label encode_lb]
          ~decode:[%e lident_of_label decode_lb] [%e caqti_wit]]

(* Generate the fields list with metainformations of a record. *)
let fields_list_of_record ~loc ~name lds =
  let label = Builder.Located.mk ~loc (prefix ~kind:`Fields name) in
  [%str
    let [%p Builder.ppat_var ~loc label] =
      let open Mazout_runtime.Intf in
      [%e label_list ~loc lds]]

(* Generate the structure for a record. *)
let str_of_record ~loc ~name lds =
  let exps = List.map lds ~f:exp_of_label_decl in
  let pats = List.map lds ~f:pat_of_label_decl in
  let caqti_wit = caqti_wit_of_record ~loc lds in
  let encode =
    [%expr
      fun [%p record_pat_of_label_decl ~loc lds] ->
        Ok [%e nested_tuple_expr ~loc exps]]
  in
  let decode =
    [%expr
      fun [%p nested_tuple_pat ~loc pats] ->
        Ok [%e record_exp_of_label_decl ~loc lds]]
  in
  generate_converter ~loc ~name ~caqti_wit ~kind:`Custom ~encode ~decode
  @ fields_list_of_record ~loc ~name lds

(* Generate the structure for an enumeration. *)
let str_of_enum ~loc ~name cstrs =
  let error_case =
    let rhs =
      [%expr
        Error (Format.sprintf "unknown case %s" [%e Builder.evar ~loc "t"])]
    in
    Builder.case ~lhs:(ppat_wildcase ~loc ~alias:(Some "t")) ~guard:None ~rhs
  in
  let caqti_wit = Builder.estring ~loc name in
  let encode =
    let encode_cases =
      List.map cstrs ~f:(fun { pcd_name = { txt; _ }; _ } ->
          let lident = Astlib.Longident.parse txt |> Loc.make ~loc in
          let lhs = Builder.ppat_construct ~loc lident None in
          let rhs = Builder.estring ~loc txt in
          Builder.case ~lhs ~guard:None ~rhs )
    in
    Builder.pexp_function ~loc encode_cases
  in
  let decode =
    let decode_cases =
      List.map cstrs ~f:(fun { pcd_name = { txt; _ }; _ } ->
          let lhs = Builder.pstring ~loc txt in
          let lident = Astlib.Longident.parse txt |> Loc.make ~loc in
          let rhs = [%expr Ok [%e Builder.pexp_construct ~loc lident None]] in
          Builder.case ~lhs ~guard:None ~rhs )
      @ [ error_case ]
    in
    Builder.pexp_function ~loc decode_cases
  in
  generate_converter ~loc ~name ~caqti_wit ~kind:`Enum ~encode ~decode

(* Test if all the constructor of a polymorphic variant type have no
   payload. *)
let str_of_polymorphic_enum ~loc ~name row_fields =
  let error_case =
    let rhs =
      [%expr
        Error (Format.sprintf "unknown case %s" [%e Builder.evar ~loc "t"])]
    in
    Builder.case ~lhs:(ppat_wildcase ~loc ~alias:(Some "t")) ~guard:None ~rhs
  in
  let caqti_wit = Builder.estring ~loc name in
  let encode =
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
    Builder.pexp_function ~loc encode_cases
  in
  let decode =
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
    Builder.pexp_function ~loc decode_cases
  in
  generate_converter ~loc ~name ~caqti_wit ~kind:`Enum ~encode ~decode

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
  | Ptype_record lds -> str_of_record ~loc ~name lds
  | Ptype_abstract -> begin
    match ptype_manifest with
    | Some { ptyp_desc = Ptyp_variant (row_fields, _, _); _ }
      when is_polymorphic_enum row_fields ->
      str_of_polymorphic_enum ~loc ~name row_fields
    | Some ty -> begin
      let caqti_wit = caqti_wit_of_ty ~loc ty in
      [%str let [%p Builder.ppat_var ~loc label] = [%e caqti_wit]]
    end
    | None -> Location.raise_errorf ~loc "ppx_deriving_mazout: unsupported type"
  end
  | Ptype_variant cstr_decls when is_enum cstr_decls ->
    str_of_enum ~loc ~name cstr_decls
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
