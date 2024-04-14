(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr
open Note
open Note_brr


type content = [ `Txt of string | `Els of El.t list signal ]

let vz_class = Jstr.v "vz-input"
let vz_float = Jstr.v "float"
let vz_int = Jstr.v "int"

let set_content el = function
| `Txt "" -> ()
| `Txt txt -> El.append_children el [El.txt' txt]
| `Els els -> Elr.def_children el els

let set_enabled ?(enabled = S.Bool.true')  el =
  let disabled_of enabled = if enabled then None else Some Jstr.empty in
  Elr.def_at At.Name.disabled (S.map disabled_of enabled) el

let make ?(at = []) ?label inputs =
  let cs = match label with
  | None -> inputs
  | Some `Txt "" -> inputs
  | Some content ->
      let label = El.label [] in
      set_content label content;
      label :: inputs
  in
  El.div ~at:(At.class' vz_class :: at) cs

let make_button ?enabled content on_click =
  let button = El.button [] in
  let act = Evr.on_el Ev.click on_click button in
  set_content button content;
  set_enabled button ?enabled;
  act, button

(* Triggers *)

let trigger ?label ?enabled v content =
  let act, button = make_button ?enabled content (Evr.stamp v) in
  act, make ?label [button]

let triggers ?label ?enabled ~init triggers =
  let but (f, content) = make_button ?enabled content (Evr.stamp f) in
  let evs, buttons = List.split (List.map but triggers) in
  let s = S.hold init (E.accum init (E.select evs)) in
  s, make ?label buttons

(* Booleans *)

let make_checkbox ?enabled ~init () =
  let i = El.input ~at:At.[type' (Jstr.v "checkbox"); if' init checked] () in
  let act = Evr.on_el Ev.change (fun _ -> El.prop El.Prop.checked i) i in
  let s = S.hold init act in
  set_enabled i ?enabled;
  s, i

let bool ?label ?enabled ~init content =
  let s, c = make_checkbox ?enabled ~init () in
  let c = match content with
  | `Txt "" -> El.label [c]
  | `Txt txt -> El.label [c; El.txt' txt]
  | `Els els ->
      let label = El.label [] in
      Elr.def_children label (S.map (List.cons c) els);
      label
  in
  s, make ?label [c]

(* Numbers *)

type number_affordance = [ `Slider | `Text_and_slider | `Text ]

let slider = At.[type' (Jstr.v "range")]

let text ~size:s =
  let size s = At.style (Jstr.v (Printf.sprintf "width: %d.75ch" (s + 2))) in
  At.[type' (Jstr.v "number"); required; if_some (Option.map size s)]

(* XXX Make {float,int}_input, start with an input we can set
   the value *)

let int_input ~on_change ?enabled ~min ~max ?step ~at init =
  let i = El.input ~at () in
  let some_int s = Some (Jstr.of_int s) in
  let set_value i v =
    El.set_prop El.Prop.value (Jstr.of_int v) i;
    ignore (Ev.dispatch (Ev.create Ev.change) (El.as_target i))
  in
  let get_value i =
    Option.value ~default:0 (Jstr.to_int (El.prop El.Prop.value i))
  in
  let () = El.set_at At.Name.value (Some (Jstr.of_int init)) i in
  let () =
    let set_min v =
      El.set_prop (El.Prop.jstr (Jstr.v "min")) (Jstr.of_int v) i;
      if get_value i < v then set_value i v;
    in
    Elr.hold_logr i (S.log ~now:true min set_min)
  in
  let () =
    let set_max v =
      El.set_prop (El.Prop.jstr (Jstr.v "max")) (Jstr.of_int v) i;
      if get_value i > v then set_value i v;
    in
    Elr.hold_logr i (S.log ~now:true max set_max)
  in
  let () = match step with
  | None -> El.set_at (Jstr.v "step") (Some (Jstr.v "1")) i
  | Some s -> Elr.def_at (Jstr.v "step") (S.map some_int s) i
  in
  let () = set_enabled i ?enabled in
  let get_value _ = get_value i in
  let act = Evr.on_el Ev.input get_value i in
  let chg = Evr.on_el Ev.change get_value i in
  act, chg, i

let float_input ~on_change ?enabled ~min ~max ?step ~at init =
  let i = El.input ~at () in
  let some_float s = Some (Jstr.of_float s) in
  let set_value i v =
    El.set_prop El.Prop.value (Jstr.of_float v) i;
    ignore (Ev.dispatch (Ev.create Ev.change) (El.as_target i))
  in
  let get_value i = Jstr.to_float (El.prop El.Prop.value i) in
  let () = El.set_at At.Name.value (Some (Jstr.of_float init)) i in
  let () =
    let set_min v =
      El.set_prop (El.Prop.jstr (Jstr.v "min")) (Jstr.of_float v) i;
      if get_value i < v then set_value i v;
    in
    Elr.hold_logr i (S.log ~now:true min set_min)
  in
  let () =
    let set_max v =
      El.set_prop (El.Prop.jstr (Jstr.v "max")) (Jstr.of_float v) i;
      if get_value i > v then set_value i v;
    in
    Elr.hold_logr i (S.log ~now:true max set_max)
  in
  let () = match step with
  | None -> El.set_at (Jstr.v "step") (Some (Jstr.v "any")) i
  | Some s -> Elr.def_at (Jstr.v "step") (S.map some_float s) i
  in
  let () = set_enabled i ?enabled in
  let get_value _ = get_value i in
  let act = Evr.on_el Ev.input get_value i in
  let chg = Evr.on_el Ev.change get_value i in
  act, chg, i

let int
    ?(on_change = false)
    ?at ?label ?text_size:size ?enabled ?(affordance = `Text_and_slider)
    ?min ?max ?step init
  =
  let min = Option.value ~default:(S.const 1) min in
  let max = Option.value ~default:(S.const 100) max in
  match affordance with
  | `Text_and_slider ->
      let a0, c0, i0 =
        int_input ~on_change ?enabled ~min ~max ?step ~at:(text ~size) init in
      let a1, c1, i1 =
        int_input ~on_change ?enabled ~min ~max ?step ~at:slider init in
      (* Keep in sync *)
      let () = Elr.set_prop El.Prop.value ~on:(E.map Jstr.of_int a1) i0 in
      let () = Elr.set_prop El.Prop.value ~on:(E.map Jstr.of_int a0) i1 in
      let e = E.select (if on_change then [c0; c1] else [c0; c1; a1]) in
      let s = S.hold init e in
      s, make ?at ?label [i0; i1]
  | `Text ->
      let act, c, i =
        int_input ~on_change ?enabled ~min ~max ?step ~at:(text ~size) init in
      let s = S.hold init (if on_change then c else act) in
      s, make ?at ?label [i]
  | `Slider ->
      let act, c, i =
        int_input ~on_change ?enabled ~min ~max ?step ~at:slider init in
      let s = S.hold init (if on_change then c else act) in
      s, make ?at ?label [i]

let float
    ?(on_change = false)
    ?at ?label ?text_size:size ?enabled ?(affordance = `Text_and_slider)
    ?min ?max ?step init
  =
  let min = Option.value ~default:(S.const 0.) min in
  let max = Option.value ~default:(S.const 1.) max in
  match affordance with
  | `Text_and_slider ->
      let a0, c0, i0 =
        float_input ~on_change ?enabled ~min ~max ?step ~at:(text ~size) init
      in
      let a1, c1, i1 =
        float_input ~on_change ?enabled ~min ~max ?step ~at:slider init
      in
      (* Keep in sync *)
      let () = Elr.set_prop El.Prop.value ~on:(E.map Jstr.of_float a1) i0 in
      let () = Elr.set_prop El.Prop.value ~on:(E.map Jstr.of_float a0) i1 in
      let e = E.select (if on_change then [c0; c1] else [c0; c1; a1]) in
      let s = S.hold init e in
      s, make ?at ?label [i0; i1]
  | `Text ->
      let act, c, i =
        float_input ~on_change ?enabled ~min ~max ?step ~at:(text ~size) init
      in
      let s = S.hold init (if on_change then c else act) in
      s, make ?at ?label [i]
  | `Slider ->
      let act, c, i =
        float_input ~on_change ?enabled ~min ~max ?step ~at:slider init
      in
      let s = S.hold init (if on_change then c else act) in
      s, make ?at ?label [i]

(* Enumerations *)

type enum_affordance = [ `Buttons | `Menu ]

let one_of ?label ?enabled ?affordance ?(eq = ( = )) enum ~init names =
  failwith "TODO"

let list ?label ?enabled ?affordance ?eq enum ~init names =
  failwith "TODO"

(* Files *)

type file_type = string

let make_file_input ?enabled ?(accept = []) mode content get =
  (* The label of file inputs can't be set… so we use
     a button that forwards its click to a hidden input element. *)
  let i =
    let accept = match accept with
    | [] -> At.void
    | types -> At.v (Jstr.v "accept") (Jstr.v (String.concat "," types))
    in
    let multiple = match mode with
    | `Files | `Dirs -> At.true' (Jstr.v "multiple") | _ -> At.void
    in
    let wdir = match mode with
    | `Dir | `Dirs -> At.true' (Jstr.v "webkitdirectory") | _ -> At.void
    in
    El.input ~at:At.[type' (Jstr.v "file"); multiple; wdir; accept] ()
  in
  let button = El.button [] in
  let forward _ =
    (* If the same file gets selected events do not refire,
         resetting the value property here works around this problem. *)
    El.set_prop El.Prop.value Jstr.empty i; El.click i
  in
  let () = ignore (Ev.listen Ev.click forward (El.as_target button)) in
  let act = Evr.on_el Ev.change (fun _ -> get i) i in
  El.set_inline_style El.Style.display (Jstr.v "none") i;
  set_content button content;
  set_enabled button ?enabled;
  act, [button; i]

let file ?label ?enabled ?accept name =
  let get i = List.hd (El.Input.files i) in
  let act, i = make_file_input ?enabled ?accept `File name get in
  act, make ?label i

type files_select = [ `Files | `Dir | `Dirs ]

let files ?label ?enabled ?accept ~select name =
  let get i = El.Input.files i in
  let act, i = make_file_input ?enabled ?accept select name get in
  act, make ?label i
