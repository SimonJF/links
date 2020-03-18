(* open Utility *)
open CommonTypes
open Sugartypes
open SourceCode

let internal_error message =
  Errors.internal_error ~filename:"desugarTemporalQueries.ml" ~message

(*
 * for (x <-- ttbl) M
 *
 * -->
 *
 * for (x <-
 *   (for x' <-- ttbl)
 *   [(data = x', valid_from = x'.valid_from, valid_to = x'.valid_to]))
 *   M
 *)
let transaction_generator pat tbl read_row from_col to_col =
  let dp = WithPos.dummy in
  let transaction_md_ty = Types.transaction_absty read_row in
  let fresh_bnd = dp (Binder.make ~name:"!bnd" ~ty:transaction_md_ty ()) in
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

  let body = dp (ListLit ([record], Some transaction_md_ty)) in
  let inner_iter = dp (Iteration ([(Table (fresh_var_pat, tbl) ) ], body, None, None)) in
  List (pat, inner_iter)

(* Here, we analyse the type of the table generator.
 * For current-time tables, we leave the pattern as-is.
 * For transaction-time tables, we use a variant of
 * Fehrenbach & Cheney's provenance translation to construct
 * a for-comprehension to add the transaction-time metadata *)
let translate_iterpatt pat phr dt =
  let open TemporalMetadata in
  let read_row = TypeUtils.table_read_type dt in
  let metadata = TypeUtils.table_metadata dt in
  match metadata with
    | Current -> Table (pat, phr)
    | TransactionTime { tt_from_field; tt_to_field } ->
        transaction_generator pat phr read_row tt_from_field tt_to_field
    | _ ->
        raise (internal_error @@
          "Metadata " ^ (TemporalMetadata.show metadata) ^ " not yet supported.")


class desugar_temporal env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! iterpatt : Sugartypes.iterpatt -> ('self_type * iterpatt) =
    function
      | Table (pat, phr) ->
          (* Here's where we do the type-directed transformation of
           * tables *)
        let (o, pat) = o#pattern pat in
        let (o, phr, dt) = o#phrase phr in
        (o, translate_iterpatt pat phr (TypeUtils.concrete_type dt))
      | ip -> super#iterpatt ip
end

(* Phrasenode: We'll probably use this to rewrite inserts and updates *)

let desugar_temporal_queries env =
  ((new desugar_temporal env) : desugar_temporal :> TransformSugar.transform)

module Typeable
  = Transform.Typeable.Make(struct
        let name = "temporal"
        let obj env =
          (desugar_temporal_queries env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)
