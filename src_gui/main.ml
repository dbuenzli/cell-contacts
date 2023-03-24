(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Note
open Brr
open Brr_note
open Gg
open Vg

let target_cells wcount obs =
  let scale = S.const None and min_max_distance = S.const None in
  Data.cell_group wcount ~scale ~min_max_distance Observation.target obs

let t_cells wcount obs setts =
  let scale = S.Option.some (S.map Data.Settings.t_scale setts) in
  let min_max_distance =
    S.Option.some (S.map Data.Settings.t_min_max_distance setts)
  in
  Data.cell_group wcount ~scale ~min_max_distance Observation.t obs

let datatable obs ~enabled ~t ~contacts =
  let last_sel = ref None in (* Where is my fix point ?! *)
  let data obs t contacts = match obs, t with
  | None, _ -> S.const None, [El.p [El.txt' "No observation loaded."]]
  | _, None -> S.const None, [El.p [El.txt' "No T cell data in observation."]]
  | Some obs, Some t ->
      match Observation.t obs with
      | None (* racy on reload !? *) ->
          Console.(log ["racy"]);
          S.const None, [El.p []]
      | Some tm ->
          let sel, el = Datatable.of_cell_group tm t ~contacts ~sel:!last_sel in
          sel, [Data.download_csv ~tm ~t ~contacts; el]
  in
  let div = El.div
      ~at:((* At.style (Jstr.v "align-items: end") :: *)
           Negsp.Layout.stack ~gap:`Zero ()) [] in
  let dt = S.l3 ~eq:( == ) data obs t contacts in
  Elr.def_children div (S.Pair.snd ~eq:( == ) dt);
  S.map (fun sel -> last_sel := sel; sel) (S.join (S.Pair.fst ~eq:( == ) dt)),
  div

let src_data_panel () =
  let wcount = Work.Counter.make () in
  let enabled = Work.Counter.is_zero wcount in
  let obs, load = Data.input_obs ~work_counter:wcount ~enabled in
  let setts, scale, in_setts = Data.Settings.input ~obs ~enabled in
  let target = target_cells wcount obs in
  let t = t_cells wcount obs setts in
  let isect = Data.intersect wcount t target in
  let contact_spec = S.map Data.Settings.contact_spec setts in
  let contacts = Data.contacts wcount contact_spec isect in
  let contact_stats = Data.contact_stats contacts in
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
  let sel, datatable = datatable obs ~enabled ~t ~contacts in
  let time, monitor_panel = Cell_monitor.v obs ~target ~t ~isect ~sel in
  El.div ~at:(Negsp.Layout.stack ()) [
    El.div ~at:(Negsp.Layout.sidebar `Start ()) [data_panel; monitor_panel];
    El.h2 [El.txt' "T cells"];
    datatable;
    El.footer ~at:[Negsp.Text.size `XS; Negsp.Text.align `End]
      [El.txt version]]

let main () =
  Work.setup ~use_worker:true;
  let body = (Document.body G.document) in
  let version = El.at (Jstr.v "data-version") body in
  let version = match version with None -> (Jstr.v "?") | Some v -> v in
  Brr.Console.(log [str "Cell tracker"; version]);
  El.append_children (Document.body G.document) [ui ~version]

let () = if Brr_webworkers.Worker.ami () then Work.run_worker () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
