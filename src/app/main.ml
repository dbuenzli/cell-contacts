(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr
open Note
open Note_brr
open Gg
open Vg

let target_cells wcount obs =
  let scale = S.const None and min_max_distance = S.const None in
  Gui_data.cell_group wcount ~scale ~min_max_distance Observation.target obs

let t_cells wcount obs setts =
  let scale = S.Option.some (S.map Gui_data.Settings.t_scale setts) in
  let min_max_distance =
    S.Option.some (S.map Gui_data.Settings.t_min_max_distance setts)
  in
  Gui_data.cell_group wcount ~scale ~min_max_distance Observation.t obs

let datatable obs ~enabled ~t ~contacts ~set_sel =
  (* Where is my fix point ?! Also the selection stuff is a mess
     hastily setup *)
  let last_sel = ref None in
  let data obs t contacts = match obs, t with
  | None, _ ->
      S.const !last_sel, [El.p [El.txt' "No observation loaded."]]
  | _, None ->
      S.const !last_sel, [El.p [El.txt' "No T cell data in observation."]]
  | Some obs, Some t ->
      match Observation.t obs with
      | None (* racy on reload !? XXX was like bad patience_map *) ->
          Console.(log ["racy"]); S.const !last_sel, [El.p []]
      | Some _tm ->
          let sel, el =
            Gui_results.of_cell_group obs t ~contacts ~sel:!last_sel ~set_sel
          in
          sel, [
            El.div ~at:(Negsp.Layout.cluster ()) [
              Gui_data.download_csv ~obs ~t ~contacts;
              (match contacts with
              | None -> El.span []
              | Some contacts ->
                  Gui_data.download_distances_csv ~obs ~t ~contacts);];
            el]
  in
  let div = El.div
      ~at:((* At.style (Jstr.v "align-items: end") :: *)
           Negsp.Layout.stack ~gap:`Zero ()) [] in
  let dt = S.l3 ~eq:( == ) data obs t contacts in
  Elr.def_children div (S.Pair.snd ~eq:( == ) dt);
  S.map (fun sel -> last_sel := sel; sel) (S.join (S.Pair.fst ~eq:( == ) dt)),
  div

let src_data_panel () =
  let wcount = Results_worker.Counter.make () in
  let enabled = Results_worker.Counter.is_zero wcount in
  let obs, load = Gui_data.input_obs ~work_counter:wcount ~enabled in
  let setts, scale, in_setts = Gui_data.Settings.input ~obs ~enabled in
  let target = target_cells wcount obs in
  let t = t_cells wcount obs setts in
  let isect = Gui_data.intersect wcount t target in
  let contact_spec = S.map Gui_data.Settings.contact_spec setts in
  let contacts = Gui_data.contacts wcount contact_spec isect in
  let contact_stats = Gui_data.contact_stats contacts in
  let at = Negsp.Layout.stack ~gap:(`Sp `XS) () in
  let cs = [
    El.div ~at [El.h2 [El.txt' "Observation"]; load; scale;];
    El.div ~at [El.h2 [El.txt' "Contacts"]; in_setts; contact_stats]]
  in
  let el = El.div ~at:(Negsp.Layout.stack ~gap:(`Sp `L) ()) cs in
  (obs, target, t, isect, contacts, enabled), el

let ui ~version =
  let (obs, target, t, isect, contacts, enabled), data_panel =
    src_data_panel ()
  in
  let datatable, monitor_panel =
    let def sel =
      let time, set_sel, monitor_panel =
        Cell_monitor.v obs ~target ~t ~isect ~sel
      in
      let set_sel = Consoler.E.log ~obs:true "Set_sel" set_sel in
      let sel', datatable = datatable obs ~enabled ~t ~contacts ~set_sel in
      let sel' = S.map Fun.id sel' in
      let () = Logr.hold (S.log sel' ignore) in
      sel', (datatable, monitor_panel)
    in
    S.fix None def
  in
  El.div ~at:(Negsp.Layout.stack ()) [
    El.div ~at:(Negsp.Layout.sidebar `Start ()) [data_panel; monitor_panel];
    El.h2 [El.txt' "T cells"];
    datatable;
    El.footer ~at:[Negsp.Text.size `XS; Negsp.Text.align `End]
      [El.txt version]]

let main () =
  Results_worker.setup ~use_worker:true;
  let body = (Document.body G.document) in
  let version = El.at (Jstr.v "data-version") body in
  let version = match version with None -> (Jstr.v "?") | Some v -> v in
  Brr.Console.(log [str "Cell tracker"; version]);
  El.append_children (Document.body G.document) [ui ~version]

let () =
  if Brr_webworkers.Worker.ami () then Results_worker.run () else main ()
