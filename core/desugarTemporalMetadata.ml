open Utility
open CommonTypes
open Sugartypes

module DesugarMetadataTypes : Types.TYPE_VISITOR =
struct
  class visitor =
      object (o)
        inherit Types.Transform.visitor as super
        method! typ =
          function
          | `Application (abs, [`Type typ]) when (Types.Abstype.name abs = "TransactionTime") ->
               let open TemporalMetadata in
               let ty =
                 StringMap.from_alist
                   [(Transaction.data_field, typ);
                    (Transaction.from_field, `Primitive Primitive.DateTime);
                    (Transaction.to_field, `Primitive Primitive.DateTime)]
                 |> Types.make_record_type in
              (ty, o)
          | t -> super#typ t
      end
end

class desugar_metadata env =
object (o: 'self_type)
  inherit (TransformSugar.transform env) as super

  (*
  method! datatype : Types.datatype -> ('self_type * Types.datatype) = fun dt ->
    let visitor = new DesugarMetadataTypes.visitor in
    let (t, _) = visitor#typ dt in
    (o, t)
    *)

  method! phrasenode : phrasenode -> ('self_type * phrasenode * Types.datatype) =
    let open TemporalOperation in
    function
      | TemporalOp (Accessor (tbl, field), phr, _replacement) as phrn ->
          (* Translation is type-preserving, so it's safe to defer to the
           * supertype's computation of the type *)
          let (_, _, ty) = super#phrasenode phrn in
          let (o, ty) = o#datatype ty in
          let (o, phr, _) = o#phrase phr in
          let field_name = TemporalOperation.field (tbl, field) in
          (o, Projection (phr, field_name), ty)
      | pn -> super#phrasenode pn
end

let desugar_temporal_metadata env =
  ((new desugar_metadata env) : desugar_metadata :> TransformSugar.transform)

module Typeable
  = Transform.Typeable.Make(struct
        let name = "temporal metadata"
        let obj env =
          (desugar_temporal_metadata env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)
