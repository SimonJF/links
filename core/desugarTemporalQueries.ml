open Utility
open CommonTypes
open Sugartypes
open SourceCode

let dp = WithPos.dummy

let internal_error message =
  Errors.internal_error ~filename:"desugarTemporalQueries.ml" ~message

(*
 * table ... = (table, fn)
 * for (x <-- ttbl) M
 *
 * -->
 *
 * for (x <-
 *   (for x' <-- ttbl.1)
 *   [(data = x', valid_from = x'.valid_from, valid_to = x'.valid_to]))
 *   M
 *)

(* fun() { for (x <-- tbl) [x] } *)
let current_accessor_fn tbl read_row =
  let fresh_bnd = dp (Binder.make ~name:"!bnd" ~ty:read_row ()) in
  let fresh_var = Binder.to_name fresh_bnd in
  let fresh_var_pat = dp (Pattern.Variable fresh_bnd) in
  let iter_body = dp (ListLit ([dp (Var fresh_var)], Some read_row)) in
  let body =
    dp (Iteration ([(Table (TableMode.current, fresh_var_pat, tbl))],
      iter_body, None, None)) in
  FunLit (Some [(Types.unit_type, Types.make_empty_closed_row ())],
    DeclaredLinearity.Unl, ([[]], body), Location.Server)


(* fun () { for (x <-- tbl) [(!data = x, !ttfrom = x.from_col, !ttto = x.to_col)] } *)
let transaction_accessor_fn tbl read_ty from_col to_col =
  let read_row =
    match read_ty with
      | `Record row -> row
      | typ -> raise (internal_error
          ("Read row type should be a record; got: " ^ (Types.show_typ typ))) in
  let transaction_md_ty = Types.transaction_absty read_ty in
  (* Iterator for the table is has the read row, extended with the
   * transaction time period types *)
  let datetime = `Primitive Primitive.DateTime in
  let iter_bnd_ty =
    `Record (Types.extend_row
      (StringMap.from_alist [(from_col, datetime); (to_col, datetime)]) read_row) in
  let fresh_bnd = dp (Binder.make ~name:"!bnd" ~ty:iter_bnd_ty ()) in
  let fresh_var = Binder.to_name fresh_bnd in
  let fresh_var_pat = dp (Pattern.Variable fresh_bnd) in
  let record =
    dp (RecordLit (
      [
        (TemporalMetadata.Transaction.data_field, dp (Var fresh_var));
        (TemporalMetadata.Transaction.from_field,
          dp (Projection (dp (Var fresh_var), from_col)));
        (TemporalMetadata.Transaction.to_field,
          dp (Projection (dp (Var fresh_var), to_col)));
      ], None)) in

  let iter_body = dp (ListLit ([record], Some transaction_md_ty)) in
  (* The transaction iterator gets "demoted" to a current iterator, as we're accessing
   * the raw record. *)
  let fn_body =
    dp (Iteration ([(Table (TableMode.current, fresh_var_pat, tbl))],
      iter_body, None, None)) in
  FunLit (Some [(Types.unit_type, Types.make_empty_closed_row ())],
    DeclaredLinearity.Unl, ([[]], fn_body), Location.Server)

let project_handle phr = dp (Projection (phr, "1"))
let project_accessor phr = dp (Projection (phr, "2"))

let accessor_result_ty tbl_ty md =
  let read_row = TypeUtils.table_read_type tbl_ty in
  let open TemporalMetadata in
  let elem_ty =
    match md with
      | Current -> read_row
      | TransactionTime _ -> Types.transaction_absty read_row
      | _ -> raise (internal_error "Valid / Bitemporal metadata not yet supported") in
  Types.make_list_type elem_ty


module TransformTableTypes : Types.TYPE_VISITOR =
struct
  class visitor =
      object (o)
        inherit Types.Transform.visitor as super
        method! typ = function
          | `Table (_read, _write, _needed, md) as tbl_ty ->
              (* TODO: I imagine this will cause problems. *)
              let md =
                match Unionfind.find md with
                  | `Undefined -> TemporalMetadata.current
                  | `Metadata md -> md in
              (* Table types need to be translated to the new pair representations *)
              let ty = StringMap.from_alist [("1", tbl_ty);
                ("2", Types.make_pure_thunk_type (accessor_result_ty tbl_ty md))]
              |> Types.make_record_type in
              (ty, o)
          | t -> super#typ t

      end
end



class desugar_temporal env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! datatype : Types.datatype -> ('self_type * Types.datatype) = fun dt ->
    let visitor = new TransformTableTypes.visitor in
    let (t, _) = visitor#typ dt in
    (o, t)

  method! phrasenode : phrasenode -> ('self_type * phrasenode * Types.datatype) =
    function
      | TableLit { name; record_type = (sugar_dt, dt);
          field_constraints; keys; temporal_metadata; database } ->

          let (o, name, _) = o#phrase name in
          let (o, keys, _) = o#phrase keys in
          let (o, database, _) = o#phrase database in
          let (o, sugar_dt) = o#sugar_datatype sugar_dt in
          let (o, dt) = TransformSugar.optionu o (fun o (r, w, n, mdv) ->
            let (o, r) = o#datatype r in
            let (o, w) = o#datatype w in
            let (o, n) = o#datatype n in
            (o, (r, w, n, mdv))) dt in

          let tbl_ty = `Table (OptionUtils.val_of dt) in

          let tbl_phr =
            dp (TableLit { name; record_type = (sugar_dt, dt);
                field_constraints; keys; temporal_metadata; database }) in

          (* Generate an accessor function and its type, based on the temporal MD *)
          let accessor_fn =
            let read_row = TypeUtils.table_read_type tbl_ty in
            begin
              let open TemporalMetadata in
              match temporal_metadata with
                | Current -> current_accessor_fn tbl_phr read_row
                | TransactionTime { tt_from_field; tt_to_field } ->
                    transaction_accessor_fn tbl_phr read_row tt_from_field tt_to_field
                | _ ->
                    raise (internal_error "ValidTime / Bitemporal metadata not yet supported.")
            end
          in
          let phr = RecordLit ([("1", tbl_phr); ("2", dp accessor_fn)], None) in
          let accessor_ty =
            Types.make_pure_thunk_type (accessor_result_ty tbl_ty temporal_metadata) in
          let pair_ty =
            (Types.make_record_type (StringMap.from_alist
              [("1", tbl_ty); ("2", accessor_ty)]))
            in
          (o, phr, pair_ty)
      | DBInsert (mode, into, labels, values, id) ->
          let (o, into, _) = o#phrase into in
          let (o, values, _) = o#phrase values in
          let (o, id, _) = TransformSugar.option o (fun o -> o#phrase) id in
          (o, DBInsert (mode, project_handle into, labels, values, id), Types.unit_type)
      | DBUpdate (mode, pat, from, where, set) ->
          let (o, from, _) = o#phrase from in
          let (o, pat) = o#pattern pat in
          let (o, where, _) = TransformSugar.option o (fun o -> o#phrase) where in
          let (o, set) =
            TransformSugar.listu o
              (fun o (name, value) ->
                 let (o, value, _) = o#phrase value in (o, (name, value))) set
          in
            (o, DBUpdate (mode, pat, project_handle from, where, set), Types.unit_type)
      | DBDelete (mode, pat, phr, where) ->
          let (o, pat) = o#pattern pat in
          let (o, phr, _) = o#phrase phr in
          let (o, where, _) = TransformSugar.option o (fun o -> o#phrase) where in
          (o, DBDelete (mode, pat, project_handle phr, where), Types.unit_type)
      | phr -> super#phrasenode phr

  method! iterpatt : Sugartypes.iterpatt -> ('self_type * iterpatt) =
    function
      | Table (_mode, pat, phr) ->
        let (o, pat) = o#pattern pat in
        let (o, phr, _dt) = o#phrase phr in
        (* Project accessor function from the table *)
        (o, List (pat, dp (FnAppl (project_accessor phr, []))))
      | ip -> super#iterpatt ip
end

let desugar_temporal_queries env =
  ((new desugar_temporal env) : desugar_temporal :> TransformSugar.transform)

module Typeable
  = Transform.Typeable.Make(struct
        let name = "temporal queries"
        let obj env =
          (desugar_temporal_queries env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)
