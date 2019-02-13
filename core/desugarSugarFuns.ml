open Sugartypes

(* Desugars SugarFuns. *)
(* By this point, every binding within a SugarFuns block will have
 * been transformed into a Fun. This pass flattens each SugarFuns
 * block into either:
   * a) a Fun, if the block is a single Fun which is non-recursive, or
   * b) a Funs, if the block is a collection of mutually-recursive functions,
   *    or if the block is a single Fun which is recursive *)

(* Simple check to see whether a function is recursive: checks whether the
 * binder occurs within the free variables of the function body. *)
let is_recursive bnd fnlit =
  StringSet.mem (name_of_binder bnd) Freevars.funlit fnlit


(* `Fun bindings must be lifted into `Funs if they are recursive. *)
let lift_funs =
object
    inherit SugarTraversals.map as super
    method! bindingnode = function
      |  `Fun (bndr, lin, (tvs, fnlit), location, dt) ->
          if is_recursive bndr fnlit then
            let fnlit = self#funlit fnlit in
            `Fun (bnd, lin, (tvs, fnlit), location, dt)
          else
            let fnlit = self#funlit fnlit in
            `Funs [(bndr, lin, ((tvs, None), fnlit), location, dt)]
      | b -> super#bindingnode b
end

(* Desugars SugarFuns blocks *)
let desugar_sugarfuns =
object
  inherit SugarTraversals.map as super
  method! bindingnode = function
    (* Empty or singleton SugarFuns should not have been constructed. *)
    | `SugarFuns [] -> assert false
    | `SugarFuns [x] -> assert false
    | `SugarFuns bs ->
        (* Recursively apply *)
        let bs = self#list (fun o -> o#binding) in
        (* All contained bindings must be of the form `Fun x. Extract these,
         * and turn them into the right form for Funs blocks. *)
        let fs =
          List.map (function
            | `Fun (bnd, lin, (tvs, fnlit), loc, dt) ->
                (bnd, lin, ((tvs, None), fnlit), loc, dt)
            (* Due to parser construction and the fact we've desugared handlers
             * into `Funs already, something must have gone seriously wrong if
             * the block contains anything but `Funs. *)
            | _ -> assert false) bs in
        `Funs fs
    | b -> super#bindingnode b
end
