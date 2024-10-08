(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr

include Results

let enc_td v = El.td ~at:At.[Negsp.Text.align `End] [El.txt v]

let thead g cols =
  let th (C c) =
    let name = if c.name_th <> "" then c.name_th else c.name in
    match c.href with
    | None -> El.th [El.txt' name]
    | Some href_def ->
        let tgt = At.v (Jstr.v "target") (Jstr.v "_blank") in
        El.th [El.a ~at:At.[tgt; href (Jstr.v href_def)] [El.txt' name]]
  in
  El.thead ~at:At.[Negsp.Text.align `End] [El.tr (List.map th cols)]


let selected = Jstr.v "selected"

let tr obs sel_tr sel_id on_click tm g contacts i =
  let cell = g.(i) in
  let track =
    match
      Trackmate.Int_map.find_opt cell.Cell.track_id tm.Trackmate.tracks_by_id
    with
    | None -> Console.(log ["PANIC! "; cell.Cell.track_id]); assert false
    | Some t -> t
  in
  let contacts = match contacts with
  | None -> None
  | Some c ->
      (* XXX I think we have a race again *)
      if i > Array.length c - 1 then None else Some c.(i)
  in
  let td cell track contacts (C c) =
    let v = c.get obs tm cell track contacts in
    enc_td (Jstr.of_string (c.enc.td v))
  in
  let tr = El.tr (List.map (td cell track contacts) cols) in
  let id = cell.track_id in
  ignore (Ev.listen Ev.click (on_click id) (El.as_target tr));
  begin match sel_id with
  | Some j when id = j -> El.set_class selected true tr; sel_tr := Some tr
  | _ -> ()
  end;
  tr

(* XXX the selection stuff is a mess redo ! In particular faster
   lookup structures. *)

let of_cell_group obs g ~contacts ~sel ~set_sel:set_sel_ev =
  let tm = Observation.t obs |> Option.get in
  let has_id id c = c.Cell.track_id = id in
  let sel_id = match sel with
  | None -> None
  | Some id -> if Array.exists (has_id id) g then Some id else None
  in
  let sel, set_sel = Note.S.create sel_id in
  let sel_tr = ref None in
  let change_sel_row id tr = match !sel_tr with
  | None ->
      El.set_class selected true tr; sel_tr := Some tr; set_sel (Some id)
  | Some sel when sel == tr -> ()
  | Some sel ->
      El.set_class selected false sel;
      El.set_class selected true tr;
      sel_tr := Some tr; set_sel (Some id)
  in
  let on_click id ev =
    let tr = El.of_jv (Ev.target_to_jv (Ev.current_target ev)) in
    change_sel_row id tr
  in
  let thead = thead g cols in
  let len = (Array.length g) in
  let rows = Array.init len (tr obs sel_tr sel_id on_click tm g contacts) in
  let tbody = El.tbody (Array.to_list rows) in
  let el =
    El.div
      ~at:At.[class' (Jstr.v "datatable"); Negsp.Text.size `S]
      [El.table [thead; tbody]]
  in
  let () =
    let do_set_sel id =
      Console.(log ["select: "; id]);
      let rec loop max i =
        if i > max then () else
        if not (has_id id g.(i)) then loop max (i + 1) else
        let row = rows.(i) in
        change_sel_row id row;
        El.scroll_into_view ~align_v:`End row (* because of header *) ;
      in
      loop (Array.length g - 1) 0
    in
    Note_brr.Elr.may_hold_logr el (Note.E.log set_sel_ev do_set_sel)
  in
  begin match !sel_tr with
  | None -> ()
  | Some tr ->
      ignore @@
      (Relax.run @@ fun () -> El.scroll_into_view ~align_v:`End tr)
  end;
  sel, el
