open Js_of_ocaml
open Jingoo
open Jg_types

let () =
  let getElementById coerce id =
    match Js.Opt.to_option @@ Dom_html.document##getElementById (Js.string id) with
    | None -> failwith id
    | Some x -> match Js.Opt.to_option @@ coerce x with
      | None -> failwith id
      | Some x -> x
  in
  let models = getElementById Dom_html.CoerceTo.textarea "models" in
  let input = getElementById Dom_html.CoerceTo.textarea "input" in
  let highlight = getElementById Dom_html.CoerceTo.pre "highlight" in
  let output = getElementById Dom_html.CoerceTo.div "output" in
  let status = getElementById Dom_html.CoerceTo.div "status" in
  let tab_models = getElementById Dom_html.CoerceTo.div "tab-models" in
  let tab_editor = getElementById Dom_html.CoerceTo.div "tab-editor" in
  let tab_preview = getElementById Dom_html.CoerceTo.div "tab-preview" in
  let nav_editor = getElementById Dom_html.CoerceTo.div "nav-editor" in
  let nav_models = getElementById Dom_html.CoerceTo.div "nav-models" in
  let nav_preview = getElementById Dom_html.CoerceTo.div "nav-preview" in
  let update fn () =
    try
      fn () ;
      status##.innerHTML := Js.string "" ;
      status##.classList##remove (Js.string "error")
    with e ->
      status##.innerHTML := Js.string @@ Printexc.to_string e ;
      status##.classList##add (Js.string "error")
  in
  let delay fn = ignore @@ Dom_html.window##setTimeout (Js.wrap_callback fn) 0. in
  let update_input () =
    try
      let txt = Js.to_string input##.value in
      highlight##.innerHTML :=
        Js.string @@
        unbox_string @@
        Jg_highlight.highlight @@
        Jg_runtime.jg_escape_html @@
        Tstr txt ;
      input##.classList##remove (Js.string "error") ;
    with e ->
      input##.classList##add (Js.string "error") ;
      raise e
  in
  let update_output () =
    let value = ref Tnull in
    let () =
      Jg_interp.from_string
        ~env:{ Jg_types.std_env with autoescape = false }
        ~output:((:=) value)
        (String.trim @@ Js.to_string models##.value) in
    let models = Jg_types.unbox_obj !value in
    output##.innerHTML := Js.string @@ Jg_template.from_string ~models (Js.to_string input##.value)
  in
  let update_selected =
    let tab_selected = ref tab_editor in
    let nav_selected = ref nav_editor in
    fun nav tab ->
      !tab_selected##.classList##remove (Js.string "active") ;
      !nav_selected##.classList##remove (Js.string "active") ;
      nav##.classList##add (Js.string "active") ;
      tab_selected := tab ;
      tab##.classList##add (Js.string "active") ;
      nav_selected := nav
  in
  nav_editor##.onclick := Dom.handler (fun _ -> update_selected nav_editor tab_editor ; Js._false) ;
  nav_models##.onclick := Dom.handler (fun _ -> update_selected nav_models tab_models ; Js._false) ;
  nav_preview##.onclick := Dom.handler (fun _ ->
      update_selected nav_preview tab_preview ;
      update update_output () ;
      Js._false) ;
  input##.oninput := Dom.handler (fun _ -> update update_input () ; Js._false) ;
  (Obj.magic input)##.oncut := Dom.handler (fun _ -> delay (update update_input) ; Js._true) ;
  (Obj.magic input)##.onpaste := Dom.handler (fun _ -> delay (update update_input) ; Js._true) ;
  update update_input ()
