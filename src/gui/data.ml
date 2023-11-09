(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr
open Note

module Settings = struct
  type t =
    { t_scale : float;
      t_min_max_distance : float;
      contact_spec : Cell.Contact.spec; }

  let t_scale s = s.t_scale
  let t_min_max_distance s = s.t_min_max_distance
  let contact_spec s = s.contact_spec
  let v t_scale t_min_max_distance contact_spec =
    { t_scale; t_min_max_distance; contact_spec }

  let scale ~enabled init =
    let min = 1. and max = 3. in
    let init = if init < min then init else if init > max then max else init in
    let label =
      S.const [Icon.arrows_pointing_out (); El.txt' "T scale factor"]
    in
    let text_size = 4 in
    let min = S.const min and max = S.const max and step = S.const 0.01 in
    Input.float ~enabled ~on_change:true
      ~label:(`Els label) ~text_size init ~min ~max ~step

  let small_light txt =
    (* XXX icon offset ! XXX negsp contrast *)
    let style = At.style (Jstr.v "color: #555; margin-left:1.6rem") in
    El.div ~at:[Negsp.Text.size `S; style] txt

  let min_max_distance ~enabled ~obs init =
    let min = 0. and max = 100. in
    let init = if init < min then init else if init > max then max else init in
    let unit = function
    | None -> "μm"
    | Some obs -> Observation.physical_unit obs
    in
    let label obs = [ Icon.trash (); El.txt' ("T dead limit " ^ unit obs)] in
    let label = `Els (S.map label obs) in
    let text_size = 4 in
    let min = S.const min and max = S.const max and step = S.const 1. in
    let txt =
      let def = Jstr.v "https://imagej.net/imagej-wiki-static/\
                        TrackMate_Algorithms.html#Max_distance_traveled."
      in
      let link =
        let target = At.v (Jstr.v "target") (Jstr.v "_blank") in
        El.a ~at:At.[target; href def; style (Jstr.v "color: #555")]
          [El.txt' "maximal distance travelled"]
      in
      let txt = [El.txt' "Minimal "; link; El.txt' " for keeping a T cell." ] in
      small_light txt
    in
    let s, el =
      Input.float ~enabled ~on_change:true ~text_size ~label init ~min ~max
        ~step
    in
    s, El.div [el; txt]

  let show_dur obs count = match obs with
  | None -> ""
  | Some o ->
      let dur, unit = Observation.dur_of_frame_count o ~count in
      Printf.sprintf "= %.1f%s" dur unit

  let min_frames_count ~obs ~enabled init =
    let min = 2 and max = 100 in
    let init = if init < min then init else if init > max then max else init in
    let label =
      `Els (S.const [Icon.clock (); El.txt' "Min. frames for stable contact"])
    in
    let min = S.const min and max = S.const max in
    let affordance = `Text in
    let count, el =
      Input.int ~affordance ~enabled ~on_change:true ~text_size:3 ~label
        init ~min ~max;
    in
    let at = Negsp.Layout.cluster ~gap:(`Sp `XXS) () in
    count, El.div ~at [el; Output.span (S.l2 show_dur obs count)]

  let allowed_overlap_gap_length ~obs ~enabled init =
    let min = 0 and max = 200 in
    let init = if init < min then init else if init > max then max else init in
    let label =
      `Els (S.const [Icon.cube_transparent ();
                     El.txt' "Allowed overlap gap length"])
    in
    let min = S.const min and max = S.const max in
    let affordance = `Text in
    let count, el =
      Input.int ~affordance ~enabled ~on_change:true ~text_size:3 ~label
        init ~min ~max;
    in
    let at = Negsp.Layout.cluster ~gap:(`Sp `XXS) () in
    let txt =
      let t =
        "Keep contact open even if min. % overlap not met during that time."
      in
      small_light [El.txt' t]
    in
    count, El.div [El.div ~at [el; Output.span (S.l2 show_dur obs count)];
                   txt]

  let min_t_overlap ~enabled init =
    let min = 0 and max = 100 in
    let init = if init < min then init else if init > max then max else init in
    let label =
      `Els
        (S.const [Icon.square_2_stack ();
                  El.txt' "Min. % overlap for contact"])
    in
    let min = S.const min and max = S.const max in
    let overlap, el =
      Input.int ~enabled ~on_change:true ~text_size:3 ~label init ~min ~max in
    let txt = small_light [El.txt' "Percentage of the T cell area."] in
    overlap, El.div [el; txt]

  let set_int st k i =
    Console.log_if_error ~use:() @@
    (Brr_io.Storage.set_item st (Jstr.v k) (Jstr.of_int i))

  let set_float st k f =
    Console.log_if_error ~use:() @@
    (Brr_io.Storage.set_item st (Jstr.v k) (Jstr.of_float f))

  let get_int st k ~init = match Brr_io.Storage.get_item st (Jstr.v k) with
  | None -> init
  | Some v -> Option.value ~default:init (Jstr.to_int v)

  let get_float st k ~init = match Brr_io.Storage.get_item st (Jstr.v k) with
  | None -> init
  | Some v -> Jstr.to_float v

  let load () =
    let st = Brr_io.Storage.local G.window in
    let t_scale = get_float st "t_scale" ~init:Cell.Group.t_scale_default in
    let t_min_max_distance =
      get_float st "t_min_max_distance" ~init:Cell.Group.t_min_max_distance
    in
    let min_frame_count =
      get_int st "min_frame_count"
        ~init:Cell.Contact.spec_default.min_frame_count
    in
    let allowed_overlap_gap_length =
      get_int st "allowed_overlap_gap_length"
        ~init:Cell.Contact.spec_default.allowed_overlap_gap_length
    in
    let min_overlap_pct =
      get_int st "min_overlap_pct"
        ~init:Cell.Contact.spec_default.min_overlap_pct
    in
    { t_scale; t_min_max_distance;
      contact_spec = { Cell.Contact.min_frame_count;
                       allowed_overlap_gap_length; min_overlap_pct } }

  let save setts =
    let st = Brr_io.Storage.local G.window in
    let () = set_float st "t_scale" setts.t_scale in
    let () = set_float st "t_min_max_distance" setts.t_min_max_distance in
    let () = set_int st "min_frame_count" setts.contact_spec.min_frame_count in
    let () =
      set_int st "allowed_overlap_gap_length"
        setts.contact_spec.allowed_overlap_gap_length
    in
    let () = set_int st "min_overlap_pct" setts.contact_spec.min_overlap_pct in
    ()

  let input ~obs ~enabled =
    let init = load () in
    let t_scale, scale_el = scale ~enabled init.t_scale in
    let t_min_max_distance, min_max_distance_el =
      min_max_distance ~obs ~enabled init.t_min_max_distance
    in
    let min_frame, min_frames_el =
      min_frames_count ~obs ~enabled init.contact_spec.min_frame_count
    in
    let allowed_overlap_gap_length, allowed_overlap_gap_length_el =
      allowed_overlap_gap_length
        ~obs ~enabled init.contact_spec.allowed_overlap_gap_length
    in
    let min_overlap, min_overlap_el =
      min_t_overlap ~enabled init.contact_spec.min_overlap_pct
    in
    let setts
        t_scale t_min_max_distance min_frame_count allowed_overlap_gap_length
        min_overlap_pct
      =
      let contact_spec =
        { Cell.Contact.min_frame_count; allowed_overlap_gap_length;
          min_overlap_pct }
      in
      let setts = { t_scale; t_min_max_distance; contact_spec } in
      save setts; setts
    in
    let setts =
      S.app (S.app ~eq:(==)
               (S.l3 ~eq:(==) setts t_scale t_min_max_distance min_frame)
               allowed_overlap_gap_length) min_overlap
    in
    let at = Negsp.Layout.stack ~gap:(`Sp `XXS) () in
    setts,
    El.div ~at [scale_el; min_max_distance_el],
    El.div ~at [ min_frames_el;
                 min_overlap_el;
                 allowed_overlap_gap_length_el; ]
end

module Load = struct
  let kind = function `T -> "T cells" | `Target -> "Target cells"
  let load_progress = function
  | None -> "…"
  | Some (p, t) ->
      let pct = truncate ((p /. t) *. 100.) in
      Fmt.str " % 3d%%" pct

  let progress = function
  | `Loading (k, f, p) -> Fmt.str "Loading %s%s" (kind k) (load_progress p)
  | `Parsing (k, _) -> Fmt.str "Parsing %s data…" (kind k)
  | `Done (k, _) -> Fmt.str "Loaded %s" (kind k)
  | `Error (k, _, e) -> Fmt.str "%s error: %s" (kind k) e
  | `No_data e -> e
  | `Obs _ -> Fmt.str "Observation loaded."

  let find_observation_files files =
    let find (t, target as acc) file =
      let name = File.name file in
      if not (Jstr.ends_with ~suffix:(Jstr.v ".xml") name) then acc else
      let name = Jstr.to_string (Jstr.lowercased name) in
      if Observation.is_t_filename name then (Some file, target) else
      if Observation.is_target_filename name then (t, Some file) else acc
    in
    List.fold_left find (None, None) files

  let trackmate_data wcount kind notify = function
  | None -> Fut.return None
  | Some file ->
      let () = Work.Counter.incr wcount in
      let open Fut.Result_syntax in
      let handle_error = function
      | Ok tm -> Some tm
      | Error e ->
          notify (`Error (kind, file, Jstr.to_string (Jv.Error.message e)));
          None
      in
      Fut.map handle_error @@
      let blob = File.as_blob file in
      let progress p = notify (`Loading (kind, file, p)) in
      let* xml = Blob.text ~progress blob in
      let () = notify (`Parsing (kind, file)) in
      Relax.run' @@ fun () ->
      let r = match Trackmate_brr.of_jstr ~file:(File.name file) xml with
      | Error _ as v -> v
      | Ok _ as v -> notify (`Done (kind, file)); v
      in
      Work.Counter.decr wcount;
      Fut.return r

  let no_data_err =
    "Could not find observation data. No t-*.xml or target-*.xml file."

  let observation wcount notify files =
    ignore @@ Fut.map notify @@ match find_observation_files files with
    | None, None -> Fut.return (`No_data no_data_err)
    | t, target ->
        let open Fut.Syntax in
        let* t = trackmate_data wcount `T notify t in
        let* target = trackmate_data wcount `Target notify target in
        Fut.return (`Obs (Observation.v ~t ~target))

end

let work_info work_count =
  let count v = if v = 0 then "" else "Processing data…" in
  let at = Negsp.Layout.cluster ~gap:(`Size (Negsp.Size.raw "0.5em")) () in
  let at = Negsp.Text.size `S :: at in
  El.div ~at
    [ Output.spinner (S.Bool.not (Work.Counter.is_zero work_count));
      Output.span (S.map count (Work.Counter.value work_count));]

let progress p =
  let log = S.hold "No observation." (E.map Load.progress p) in
  El.p ~at:[Negsp.Text.size `S] [Output.span log]

let input_dir_files ~enabled () =
  let label = [Icon.database (); El.txt' "Load observation directory…"] in
  Input.files ~enabled ~select:`Dir (`Els (S.const label))

let input_obs ~enabled ~work_counter:wcount =
  let work_info = work_info wcount in
  let p, notify = E.create () in
  let log = progress p in
  let obs = E.map (function `Obs (Ok o) -> (Some o) | _ -> None) p in
  let files, but = input_dir_files ~enabled () in
  let but = El.div ~at:(Negsp.Layout.cluster ()) [but; work_info] in
  Logr.may_hold (E.log files (Load.observation wcount notify));
  S.hold None obs,
  El.div ~at:(Negsp.Layout.stack ~gap:(`Sp `XXS) ()) [but; log]

(* Groups *)

let timer tt = let tag = Jstr.v tt in Console.time tag; tag

let make_group
    ?(log = fun err -> Console.(error [str err])) ?scale ?min_max_distance =
  function
  | None -> Fut.return None
  | Some tm ->
      let tt = timer "Page: making group" in
      let r = match Cell.Group.of_trackmate ?scale ?min_max_distance tm with
      | Error e -> log e; None
      | Ok g -> (Some g)
      in
      Console.time_end tt;
      Fut.return r

let cell_group wcount ~scale ~min_max_distance group obs =
  let t3 s0 s1 s2 = S.l3 (fun v0 v1 v2 -> (v0, v1, v2)) s0 s1 s2 in
  let cells (o, scale, min_max_distance) =
    Work.Counter.incr wcount;
    let g =
      make_group ?scale ?min_max_distance (Option.join (Option.map group o))
    in
    Fut.map (fun v -> Work.Counter.decr wcount; v) g
  in
  let filler = Fun.const None in
  Relax.S.patience_map ~filler cells (t3 obs scale min_max_distance)

let intersect wcount t target =
  let intersect wcount (t, target) =
    Work.Counter.incr wcount;
    let tt = timer "Page: intersecting" in
    let f = match t, target with
    | Some t, Some target ->
        let ret = function
        | Ok (isect, _) -> Some isect
        | Error e ->
            (* FIXME show user or rather change isect interface *)
            Console.(error [str "Isect error:"; str e]);
            None
        in
        Fut.map ret (Work.send (Work.Cell_isect (t, target)))
    | _ -> Fut.return None
    in
    Fut.map (fun r -> Work.Counter.decr wcount; Brr.Console.time_end tt;r) f
  in
  let filler = Fun.const None in
  Relax.S.patience_map ~filler (intersect wcount) (S.Pair.v t target)

(* Contacts *)

let contacts wcount contact_spec isect =
  let contacts wcount (contact_spec, isect) = match isect with
  | None -> Fut.return None
  | Some _ ->
      Work.Counter.incr wcount;
      let tt = timer "Page: contacts" in
      let f = Work.send (Work.Cell_contacts contact_spec) in
      Fut.map (fun r -> Work.Counter.decr wcount; Brr.Console.time_end tt;r) f
  in
  Relax.S.map (contacts wcount) ~init:None (S.Pair.v contact_spec isect)

let contact_stats contacts = (* FIXME Output.div *)
  let stats = function
  | None -> []
  | Some cs ->
      let stats = Cell.Contact.stats cs in
      let pct =
        truncate (((float stats.Cell.Contact.num_contacting) /.
                   (float (Array.length cs))) *. 100.)
      in

      [ El.p [El.txt' (Printf.sprintf "%d (%d%%) T cells contact."
                         stats.Cell.Contact.num_contacting pct)];
        El.p [El.txt' (Printf.sprintf "Most active T cell visits %d targets."
                         stats.Cell.Contact.max_target_contacts)]]
  in
  let div = El.div ~at:[At.style (Jstr.v "margin-top: var(--sp_s)")] [] in
  let () = Note_brr.Elr.def_children div (S.map stats contacts) in
  div

let download_csv ~tm ~t ~contacts  =
  let at = At.(v (Jstr.v "download") (Jstr.v "t-cells.csv") ::
               class' (Jstr.v "download") :: Negsp.Text.size `S ::
               Negsp.Layout.with_icon ()) in
  let el =
    El.a ~at [El.label [Icon.document_arrow_down (); El.txt' ".csv file"]]
  in
  ignore (Ev.listen Ev.click (fun _ ->
      let data = Jstr.v (Datatable.to_csv tm t contacts) in
      let data = Result.get_ok (Brr.Uri.encode_component data) in
      let data_url = Jstr.(v "data:text/csv;charset-utf-8," + data) in
      El.set_prop (El.Prop.jstr (Jstr.v "href")) data_url el;)
      (El.as_target el));
  el
