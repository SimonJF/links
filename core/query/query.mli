open Utility
open CommonTypes

module Lang :
sig

  type base_type = | Bool | Char | Float | Int | String

  type tag = int
      [@@deriving show]

  type t =
      | For       of tag option * (Var.var * t) list * t list * t
      | If        of t * t * t
      | Table     of Value.table
      | Database  of (Value.database * string)
      | Singleton of t
      | Concat    of t list
      | Record    of t StringMap.t
      | Project   of t * string
      | Erase     of t * StringSet.t
      | Variant   of string * t
      | XML       of Value.xmlitem
      | Apply     of t * t list
      | Closure   of (Var.var list * Ir.computation) * env
      | Case      of t * (Var.binder * t) StringMap.t * (Var.binder * t) option
      | Primitive of string
      | Var       of Var.var * Types.datatype StringMap.t
      | Constant  of Constant.t
  and env = { venv: Value.env; qenv: t Env.Int.t; policy: QueryPolicy.t }
      [@@deriving show]

  val eta_expand_var : Var.var * Types.datatype StringMap.t -> t
  val reduce_where_then : t * t -> t
  val reduce_and : t * t -> t
end

module type QUERY_VISITOR =
sig
  class visitor :
  object ('self_type)
    method query : Lang.t -> ('self_type * Lang.t)
    method tag : Lang.tag -> ('self_type * Lang.tag)
    method binder : Var.binder -> ('self_type * Var.binder)

    method option :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
        'a option ->
        ('self_type * ('a option))

    method list :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
        'a list ->
        ('self_type * ('a list))
  end
end

module Transform : QUERY_VISITOR

val unbox_xml : Lang.t -> Value.xmlitem

val used_database : Lang.t -> Value.database option

val string_of_t : Lang.t -> string
val show_base :   Value.database -> Lang.t -> string

val type_of_expression : Lang.t -> Types.datatype

val default_of_base_type : Primitive.t -> Lang.t

val value_of_expression : Lang.t -> Value.t
val expression_of_base_value : Value.t -> Lang.t

val labels_of_field_types : 'a Utility.StringMap.t -> Utility.StringSet.t
val record_field_types : Types.datatype -> Types.datatype StringMap.t
val table_field_types : Value.table -> Types.typ Utility.StringMap.t
val is_list : Lang.t -> bool

val sql_of_query : Lang.t -> Sql.query

(* Specific to nested queries *)
type let_clause = Var.var * Lang.t * Var.var * Lang.t
type let_query = let_clause list
val sql_of_let_query : let_query -> Sql.query

module Eval :
sig
  val env_of_value_env : QueryPolicy.t -> Value.env -> Lang.env
  val bind : Lang.env -> Env.Int.name * Lang.t -> Lang.env
  val computation : Lang.env -> Ir.computation -> Lang.t
  val eval : QueryPolicy.t -> Value.t Value.Env.t -> Ir.computation -> Lang.t
end

module ValidTime :
sig
  val compile_update :
    Ir.valid_time_update ->
    Value.database ->
    Value.env ->
    ((Ir.var * string * Types.datatype StringMap.t) *
      Ir.computation option * Ir.computation) ->
    string (* from field *) ->
    string (* to field *) ->
    Sql.query

  val compile_delete :
    Ir.valid_time_deletion ->
    Value.database ->
    Value.env ->
    ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) ->
    string -> (* 'from' field *)
    string -> (* 'to' field *)
    Sql.query
end

module TransactionTime :
sig
  val compile_update :
    Value.env ->
      ((Ir.var * string * Types.datatype StringMap.t) *
        Ir.computation option * Ir.computation) ->
      string -> (* transaction time from field *)
      string -> (* transaction time to field *)
      Sql.query

  val compile_delete :
    Value.database ->
    Value.env ->
    ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) ->
    string -> (* 'to' field *)
    Sql.query
end

val compile_update :
  Value.database ->
  Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) *
    Ir.computation option * Ir.computation) ->
  Sql.query

val compile_delete :
  Value.database ->
  Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) ->
  Sql.query

(* Note that temporal insertions defer unwrapping, since
 * valid-time insertions insert valid-time metadata rather than raw rows *)
val temporal_insert :
  string -> (* Table name *)
  Value.t -> (* Values and columns (wrapped) *)
  Value.TemporalState.t ->
  Sql.query

val insert :
  string -> (* Table name *)
  string list -> (* Field names *)
  Value.t list list -> (* Values *)
  Sql.query

val row_columns_values :
  Value.t -> string list * Value.t list list
