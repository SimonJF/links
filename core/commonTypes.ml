(* This module contains common datatypes used in ASTs in both the frontend
   (Sugartypes) and typechecker (Types). *)

open Utility
open CalendarLib

module Linearity = struct
  type t = Any | Unl
    [@@deriving eq,show]

  let is_any = function
    | Any -> true
    | _   -> false

  let is_nonlinear = function
    | Unl -> true
    | _   -> false

  let to_string = function
    | Any -> "Any"
    | Unl -> "Unl"

  let min l r =
    match l, r with
    | Unl, _ | _, Unl -> Unl
    | Any, Any -> Any
end

(* Convenient aliases for constructing values *)
let lin_any = Linearity.Any
let lin_unl = Linearity.Unl

module DeclaredLinearity = struct
  type t = Lin | Unl
    [@@deriving show]

  let is_linear = function
    | Lin -> true
    | _   -> false

  let is_nonlinear = function
    | Unl -> true
    | _   -> false
end

(* Convenient aliases for constructing values *)
let dl_lin = DeclaredLinearity.Lin
let dl_unl = DeclaredLinearity.Unl

module Restriction = struct
  type t =
    | Any
    | Base
    | Mono
    | Session
    | Effect
    [@@deriving eq,show]

  let is_any = function
    | Any -> true
    | _   -> false

  let is_base = function
    | Base -> true
    | _    -> false

  let is_mono = function
    | Mono -> true
    | _    -> false

  let is_session = function
    | Session -> true
    | _       -> false

  let is_effect = function
    | Effect -> true
    | _      -> false

  let to_string = function
    | Any     -> "Any"
    | Base    -> "Base"
    | Mono    -> "Mono"
    | Session -> "Session"
    | Effect  -> "Eff"

  let min l r =
    match l, r with
    | Any, Any         -> Some Any
    | Mono, Mono       -> Some Mono
    | Session, Session -> Some Session
    | Effect, Effect   -> Some Effect
    | Base, Base       -> Some Base
    | x, Any | Any, x  -> Some x (* Any will narrow to anything. *)
    | Base, Mono | Mono, Base -> Some Base (* Mono can narrow to Base. *)
    | Session, Mono | Mono, Session -> Some Session (* Super dubious, but we don't have another way*)
    | _ -> None
end

(* Convenient aliases for constructing values *)
let res_any     = Restriction.Any
let res_base    = Restriction.Base
let res_mono    = Restriction.Mono
let res_session = Restriction.Session
let res_effect  = Restriction.Effect

module Subkind = struct
  type t = Linearity.t * Restriction.t
    [@@deriving eq,show]

  let to_string (lin, res) =
    Printf.sprintf "(%s,%s)" (Linearity.to_string   lin)
                             (Restriction.to_string res)
end

module PrimaryKind = struct
  type t =
    | Type
    | Row
    | Presence
    [@@deriving show,eq]

  let to_string = function
    | Type     -> "Type"
    | Row      -> "Row"
    | Presence -> "Presence"
end

(* Convenient aliases for constructing values *)
let pk_type     = PrimaryKind.Type
let pk_row      = PrimaryKind.Row
let pk_presence = PrimaryKind.Presence

module Kind = struct
  type t = PrimaryKind.t * Subkind.t
    [@@deriving eq,show]
end

module Quantifier = struct
  type t = int * Kind.t
    [@@deriving show]

  let to_var = function
    | (var, _) -> var

  let to_kind : t -> Kind.t = function
    | (_, k) -> k

  let to_primary_kind : t -> PrimaryKind.t = function
    | (_, (pk, _)) -> pk

  let to_subkind : t -> Subkind.t = function
    | (_, (_, sk)) -> sk

  let to_string = Format.asprintf "%a" pp

  let eq : t -> t -> bool = fun lvar rvar ->
    to_var lvar = to_var rvar
end

module Location = struct
  type t = Client | Server | Unknown
    [@@deriving show]

  let is_client = function
    | Client -> true
    | _      -> false

  let is_server = function
    | Server -> true
    | _      -> false

  let is_unknown = function
    | Unknown -> true
    | _      -> false

  let to_string = function
    | Client -> "client"
    | Server -> "server"
    | Unknown -> "unknown"
end

(* Convenient aliases for constructing values *)
let loc_client  = Location.Client
let loc_server  = Location.Server
let loc_unknown = Location.Unknown

module Freedom = struct
  type t = [`Flexible | `Rigid]
    [@@deriving show]
end

module Name = struct
  type t = string
    [@@deriving show]
end

module ForeignLanguage = struct
  type t =
    | JavaScript
    [@@deriving show]

  let of_string = function
    | "javascript" -> JavaScript
    | _ -> raise (Invalid_argument "of_string")

  let to_string = function
    | JavaScript -> "javascript"
end

module Primitive = struct
  type t = Bool | Int | Char | Float | XmlItem | DB | String | DateTime
    [@@deriving show]

  let to_string = function
    | Bool    -> "Bool"
    | Int     -> "Int"
    | Char    -> "Char"
    | Float   -> "Float"
    | XmlItem -> "XmlItem"
    | DB      -> "Database"
    | String  -> "String"
    | DateTime -> "DateTime"
end

module Timestamp = struct
  type t = Timestamp of CalendarShow.t | Forever
  [@@deriving show, ord]

  let timestamp ts = Timestamp ts
  let forever = Forever
  let to_string = function
    | Timestamp ts -> Printf.sprintf "'%s'" (Printer.Calendar.to_string ts)
    | Forever -> "'infinity'"
end

module Constant = struct
  type t =
    | Float  of float
    | Int    of int
    | Bool   of bool
    | String of string
    | Char   of char
    | DateTime of Timestamp.t
      [@@deriving show, ord]

  let type_of = function
    | Float  _ -> Primitive.Float
    | Int    _ -> Primitive.Int
    | Bool   _ -> Primitive.Bool
    | Char   _ -> Primitive.Char
    | String _ -> Primitive.String
    | DateTime _ -> Primitive.DateTime

  module DateTime = struct
    let now () = DateTime (Timestamp.timestamp (CalendarShow.now()))
    let forever = DateTime (Timestamp.forever)
  end

  (* SQL standard for escaping single quotes in a string *)
  let escape_string s =
    Str.global_replace (Str.regexp "'") "''" s

  (* This function is actually specific to database query generation; it should
     be moved to the database module(s). *)
  let to_string = function
    | Bool value  -> string_of_bool value
    | Int value   -> string_of_int value
    | Char c      -> "'"^ Char.escaped c ^"'"
    | String s    -> "'" ^ escape_string s ^ "'"
    | Float value -> string_of_float' value
    | DateTime ts -> Timestamp.to_string ts
end

module QueryPolicy = struct
  type t = Flat | Nested | Default
    [@@deriving show]
end

module TableMode = struct
  type t = Current | Transaction | Valid | Bitemporal
  [@@deriving show]

  let current = Current
  let transaction = Transaction
  let valid = Valid
  let bitemporal = Bitemporal
end

module TemporalMetadata = struct
  type t =
    | Current
    | TransactionTime of { tt_from_field: string; tt_to_field: string }
    | ValidTime       of { vt_from_field: string; vt_to_field: string }
    | Bitemporal      of
      { tt_from_field: string; tt_to_field: string;
        vt_from_field: string; vt_to_field: string }

  let current = Current

  let transaction_time tt_from_field tt_to_field =
    TransactionTime { tt_from_field; tt_to_field }

  let valid_time vt_from_field vt_to_field =
    ValidTime { vt_from_field; vt_to_field }

  let bitemporal tt_from_field tt_to_field vt_from_field vt_to_field =
    Bitemporal { tt_from_field; tt_to_field; vt_from_field; vt_to_field }

  let show =
    let open Printf in
    function
    | Current -> "Current"
    | TransactionTime { tt_from_field; tt_to_field } ->
        sprintf "Transaction(%s, %s)" tt_from_field tt_to_field
    | ValidTime { vt_from_field; vt_to_field } ->
        sprintf "Valid(%s, %s)" vt_from_field vt_to_field
    | Bitemporal { tt_from_field; tt_to_field; vt_from_field; vt_to_field } ->
        sprintf "Bitemporal(%s, %s, %s, %s)" tt_from_field tt_to_field
          vt_from_field vt_to_field

  let show_term =
    let open Printf in
    function
    | Current -> "current"
    | TransactionTime { tt_from_field; tt_to_field } ->
        sprintf "transaction_time(%s, %s)" tt_from_field tt_to_field
    | ValidTime { vt_from_field; vt_to_field } ->
        sprintf "valid_time(%s, %s)" vt_from_field vt_to_field
    | Bitemporal { tt_from_field; tt_to_field; vt_from_field; vt_to_field } ->
        sprintf "bitemporal(%s, %s, %s, %s)" tt_from_field tt_to_field
          vt_from_field vt_to_field


  let pp fmt x = Format.pp_print_string fmt (show x)

  module Transaction = struct
    let data_field = "!ttdata"
    let from_field = "!ttfrom"
    let to_field = "!ttto"
  end
end

module TemporalOperation = struct
  type t =
    | TransactionData
    | TransactionFrom
    | TransactionTo
    [@@deriving show]

  let name = function
    | TransactionData -> "ttData"
    | TransactionFrom -> "ttFrom"
    | TransactionTo   -> "ttTo"

  let field = function
    | TransactionData -> TemporalMetadata.Transaction.data_field
    | TransactionFrom -> TemporalMetadata.Transaction.from_field
    | TransactionTo   -> TemporalMetadata.Transaction.to_field
end
