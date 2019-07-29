open CommonTypes.Constant
open SourceCode.WithPos
open Sugartypes

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

(* Inside a VDom node, rewrite all XML to the appropriate MVU constructors *)
let html_empty pos =
  make ~pos (ConstructorLit("HTMLEmpty", None, None))

(* Generates MvuHTML.concat. Precondition: hs are all desugared. *)
let html_concat pos hs =
  let wp x = make ~pos x in
  wp (FnAppl(wp (QualifiedVar ["MvuHTML"; "concat"]), [hs]))

let desugar_vdom =
  object(self)
    inherit SugarTraversals.map as super

    method! phrase =
      function
        | { node=TextNode str; pos } ->
            let wp x = make ~pos x in
            wp (ConstructorLit("HTMLRaw", Some(wp (Constant (String str))), None))
        | { node=Xml (name, _attrs, _, children); pos } ->
            let wp x = make ~pos x in
            let children =
              begin
              match children with
                | [] -> html_empty pos
                | xs ->
                    let xs = self#list (fun o -> o#phrase) xs in
                    html_concat pos (wp (ListLit (xs, None)))
              end in

            (* Temporary -- need to desugar attributes properly *)
            let attrs = wp (ConstructorLit("AttrEmpty", None, None)) in
            let record_fields =
              [("tagName", wp (Constant (String name)));
               ("attrs", attrs);
               ("children", children)] in
            let record_lit = wp (RecordLit(record_fields, None)) in
            wp (ConstructorLit("HTMLTag", Some(record_lit), None))
        | {node = VDom p; _} -> super#phrase p
        | p -> super#phrase p

 end

let desugar =
  object
    inherit SugarTraversals.map as super

    method! phrase =
      function
        | {node=(VDom p); _} -> desugar_vdom#phrase p
        | p -> super#phrase p
  end




