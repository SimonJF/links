open CommonTypes.Constant
open SourceCode.WithPos
open Sugartypes
open Utility

(* This pass desugars VDom notation to the appropriate MVU constructors.
 * vdom
 *  <html>
 *    <span class="header">Hello, world!</b>
 *    <button e:onClick="{fun() { Submit }}">Submit</button>
 *  </html>
 *
 *  -->
 *
 *  HTMLTag((tagName="html", attrs=AttrEmpty, children=
 *    HTMLAppend(HTMLTag(tagName="span", attrs=AttrAttribute("class", "header"), children=HTMLEmpty),
 *               HTMLTag(tagName="button", attrs=onClick(fun() { Submit }), children=HTMLRaw("Submit")))))
 *)

(* Generates MvuHTML.concat. Precondition: hs are all desugared. *)
let html_concat pos hs =
  let wp x = make ~pos x in
  wp (FnAppl(wp (QualifiedVar ["MvuHTML"; "concat"]), [hs]))

(* Generates MvuAttr.concat. Precondition: attrs are all desugared. *)
let attrs_concat pos attrs =
  let wp x = make ~pos x in
  wp (FnAppl(wp (QualifiedVar ["MvuAttr"; "concat"]), [attrs]))


let desugaring_error pos message =
  let open Errors in
  desugaring_error ~pos ~stage:DesugarVDom ~message

  (* FIXME: NEED TO DESUGAR ATTRIBUTE VALUES *)
let desugar_attribute pos (k, v) =
  let supported_event_handlers =
    ["onClick"; "onMouseMove"; "onMouseDown"; "onMouseUp"; "onMouseEnter";
     "onMouseLeave"; "onDoubleClick"; "onMouseOver"; "onMouseOut"; "onFocus";
     "onBlur"; "onKeyUp"; "onKeyDown"; "onKeyPress"; "onInput"; "onChange";
     "onKey"; "onEnter"] in
  let is_supported = (flip (List.mem)) supported_event_handlers in
  let wp x = make ~pos x in
  let (attr_val: phrase) =
    match v with
      | [x] -> x
      | _ ->
        raise (desugaring_error pos "Only single attribute values are supported in VDom.") in
  (* If attribute is e-prefixed, then check to see whether it's supported, and if so
   * attach the handler. Otherwise just add an attribute as a K-V pair. *)
  if StringUtils.string_starts_with k "e:" then
    let name = String.sub k 2 ((String.length k) - 2) in
    begin
      if is_supported name then
        (* No guarantees that the value isn't garbage, of course, but that will
         * cause a type error later. *)
        wp (FnAppl (wp (QualifiedVar ["MvuAttrs"; name]), [attr_val]))
      else
        let supported_str = String.concat ", " supported_event_handlers in
        let message =
          Printf.sprintf "Unsupported `e:` attribute %s. Supported: %s"
            name supported_str in
        raise (desugaring_error pos message)
    end
  else
    wp (FnAppl(wp (QualifiedVar ["MvuAttr"; "attr"]),
      [wp (Constant (String k)); attr_val]))

let desugar_vdom pos =
  object(self)
    inherit SugarTraversals.map as super

    method! phrase =
      function
        | {node=TextNode str; _} ->
            let wp x = make ~pos x in
            wp (ConstructorLit("HTMLRaw", Some(wp (Constant (String str))), None))
        | {node=Xml (name, attrs, _, children); _} ->
            let wp x = make ~pos x in
            (* Desugar children *)
            let children =
              let xs = self#list (fun o -> o#phrase) children in
              html_concat pos (wp (ListLit (xs, None))) in

            (* Desugar attributes *)
            let attrs =
              attrs_concat
                pos (wp (ListLit (List.map (desugar_attribute pos) attrs, None))) in
            (*
            let attrs = attr_concat (
              wp (ConstructorLit("AttrEmpty", None, None)) in
            *)

            let record_fields =
              [("tagName", wp (Constant (String name)));
               ("attrs", attrs);
               ("children", children)] in
            let record_lit = wp (RecordLit(record_fields, None)) in
            wp (ConstructorLit("HTMLTag", Some(record_lit), None))
        | {node=VDom p; _} -> super#phrase p
        | p -> super#phrase p

 end

let desugar =
  object
    inherit SugarTraversals.map as super

    method! phrase =
      function
        | {node=(VDom p); pos} -> (desugar_vdom pos)#phrase p
        | p -> super#phrase p
  end




