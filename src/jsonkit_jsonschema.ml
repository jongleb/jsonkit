let schema_version = "https://json-schema.org/draft/2020-12/schema"

type t = Jsonkit_jsonschema_classify.t

let classify = Jsonkit_jsonschema_classify.classify
let declassify = Jsonkit_jsonschema_classify.declassify

let find_named name =
  List.find_map (fun (candidate, value) ->
      if String.equal candidate name then Some value else None)

let has_name name = List.exists (String.equal name)

let merge_scope ~reserved_names ~existing definitions (own, content) =
  let defs_prefix = "#/$defs/" in
  let rec rename_refs renames : t -> t = function
    | `Assoc fields ->
        `Assoc
          (List.map
             (function
               | "$ref", `String ref ->
                   let ref =
                     Option.value ~default:ref
                       (List.find_map
                          (fun (name, fresh) ->
                            if String.equal ref (defs_prefix ^ name) then
                              Some (defs_prefix ^ fresh)
                            else None)
                          renames)
                   in
                   "$ref", `String ref
               | key, value -> key, rename_refs renames value)
             fields)
    | `List values -> `List (List.map (rename_refs renames) values)
    | (`Null | `Bool _ | `Int _ | `Float _ | `String _) as atom -> atom
  in
  let equal_schema left right =
    Jsonkit_json.equal (declassify left) (declassify right)
  in
  let previous name =
    match find_named name definitions with
    | Some schema -> Some schema
    | None -> find_named name existing
  in
  let duplicates, conflicts =
    List.fold_left
      (fun (duplicates, conflicts) (name, schema) ->
        match previous name with
        | Some previous when equal_schema previous schema ->
            name :: duplicates, conflicts
        | Some _ -> duplicates, name :: conflicts
        | None when has_name name reserved_names ->
            duplicates, name :: conflicts
        | None -> duplicates, conflicts)
      ([], []) own
  in
  let rec fresh reserved_names name n =
    let candidate = Printf.sprintf "%s_%d" name n in
    if has_name candidate reserved_names then
      fresh reserved_names name (n + 1)
    else candidate
  in
  let rec rename_conflicts reserved_names = function
    | [] -> []
    | name :: names ->
        let renamed = fresh reserved_names name 2 in
        (name, renamed)
        :: rename_conflicts (renamed :: reserved_names) names
  in
  let renames =
    rename_conflicts
      (reserved_names @ List.map fst (definitions @ own))
      (List.rev conflicts)
  in
  let rename =
    match renames with [] -> Fun.id | _ -> rename_refs renames
  in
  let own =
    List.filter_map
      (fun (name, schema) ->
        if has_name name duplicates then None
        else
          Some
            ( Option.value ~default:name (find_named name renames),
              rename schema ))
      own
  in
  definitions @ own, rename content

let rec hoist_definitions reserved_names definitions :
    t -> (string * t) list * t = function
  | `Assoc fields -> (
      match find_named "$defs" fields with
      | Some (`Assoc own)
        when
          (match find_named "$id" fields with
          | Some (`String id) ->
              String.starts_with ~prefix:"file://" id
          | Some _ -> false
          | None -> true) ->
          let inherited =
            reserved_names @ List.map fst definitions
          in
          let nested = List.map fst own @ inherited in
          let rest =
            List.filter
              (fun (key, _) ->
                not (String.equal key "$defs" || String.equal key "$id"))
              fields
          in
          let inner, own = hoist_fields nested [] own in
          let inner, rest = hoist_fields nested inner rest in
          let inner, content =
            merge_scope ~reserved_names:inherited
              ~existing:definitions inner (own, `Assoc rest)
          in
          merge_scope ~reserved_names ~existing:[] definitions
            (inner, content)
      | Some (`Assoc _) -> definitions, `Assoc fields
      | _ ->
          let definitions, fields =
            hoist_fields reserved_names definitions fields
          in
          definitions, `Assoc fields)
  | `List values ->
      let definitions, values =
        List.fold_left_map
          (fun definitions value ->
            hoist_definitions reserved_names definitions value)
          definitions values
      in
      definitions, `List values
  | (`Null | `Bool _ | `Int _ | `Float _ | `String _) as atom ->
      definitions, atom

and hoist_fields reserved_names definitions fields =
  List.fold_left_map
    (fun definitions (key, value) ->
      let definitions, value =
        hoist_definitions reserved_names definitions value
      in
      definitions, (key, value))
    definitions fields

let make ?id ?title ?description ?(definitions = []) types =
  let definitions, types = hoist_definitions [] definitions types in
  let fields = match types with `Assoc fields -> fields | _ -> [] in
  let metadata =
    List.filter_map
      (fun x -> x)
      [
        Some ("$schema", `String schema_version);
        (match id with
        | None -> None
        | Some id -> Some ("$id", `String id));
        (match title with
        | None -> None
        | Some title -> Some ("title", `String title));
        (match description with
        | None -> None
        | Some description -> Some ("description", `String description));
        (match definitions with
        | [] -> None
        | defs -> Some ("$defs", `Assoc defs));
      ]
  in
  `Assoc (metadata @ fields)

module Classify = Jsonkit_jsonschema_classify

(* Defines the main jsonschema primitives for Jsonkit *)
module Primitives = Jsonkit_jsonschema_primitives.Jsonkit
module Yojson_primitives = Jsonkit_jsonschema_primitives.Yojson
