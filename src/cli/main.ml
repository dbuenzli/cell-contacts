(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open Gg

let ( let* ) = Result.bind
let () = B0_std.Log.set_level Log.Info
let time fm f = Log.time ~level:Log.Info fm f

let intersections no_isect obs ~t_scale ~t_min_max_distance =
  let* target = match Observation.target obs with
  | None -> Ok None
  | Some target -> Result.map Option.some (Cell.Group.of_trackmate target)
  in
  let* t = match Observation.t obs with
  | None -> Ok None
  | Some t ->
      Result.map Option.some
        (Cell.Group.of_trackmate ~scale:t_scale
           ~min_max_distance:t_min_max_distance t)
  in
  let* isect =
    if no_isect then Ok None else match t, target with
    | Some t, Some target ->
        time (fun r m ->
            let errs = match r with
            | Ok None -> 0 | Ok (Some (_, e)) -> e | Error _ -> 1
            in
            m "Intersected groups %s (%d errors)" (Observation.id obs) errs )
          (fun () ->
             (Result.map Option.some (Cell.Group.intersections t target)))
    | _ -> Ok None
  in
  Ok (t, target, isect)

let to_pdf ~outf obs target t isect =
  time (fun _ m -> m "Rendered %a" Fpath.pp outf) @@ fun () ->
  Result.join @@
  match target, t with
  | None, None -> assert (false);
  | Some t, None | None, Some t ->
      let imgs = Cell_img.group t in
      Os.File.write_with_oc ~force:false ~make_path:true outf @@ fun oc ->
      Cell_img.render_pdf ~dst:oc (Observation.ref obs) imgs
  | Some target, Some t ->
      let imgs = Cell_img.groups t target (Option.map fst isect) in
      Os.File.write_with_oc ~force:false ~make_path:true outf @@ fun oc ->
      Cell_img.render_pdf ~dst:oc (Observation.ref obs) imgs

let results
    ~out_fmt ~obs_dir ~outf ~no_isect ~t_scale ~t_min_max_distance
    ~contact_spec ~no_normalize
  =
  Log.if_error ~use:1 @@
  let* dir = Fpath.of_string obs_dir in
  let* obss = Cli_data.load_observations dir in
  match out_fmt with
  | `Pdf ->
      (* XXX we no longer have the time to something smart here. Just take the
         first observation. *)
      begin match obss with
      | [] -> Error "No observation found"
      | obs :: obss ->
          if obss <> [] then Log.warn begin
              fun m -> m "Multiple observations only rendering the first one"
            end;
          let* t, target, isect =
            intersections no_isect obs ~t_scale ~t_min_max_distance
          in
          let* () = to_pdf ~outf obs target t isect in
          Ok 0
      end
  | `Csv | `Csv_dist | `Json_dist as fmt ->
      let rec add_obs ~err ~headers acc = function
      | [] -> err, acc
      | obs :: obss ->
          let res =
            let* t, target, isect =
              intersections no_isect obs ~t_scale ~t_min_max_distance
            in
            let* t = match t with
            | None -> Error "missing t-cells" | Some t -> Ok t
            in
            let* target = match target with
            | None -> Error "missing target cells" | Some target -> Ok target
            in
            let* isects = match isect with
            | None -> Error "missing intersections"
            | Some (isects, _errs) -> Ok isects
            in
            let contacts = Cell.Contact.find contact_spec ~t ~target ~isects in
            match fmt with
            | `Csv ->
                Ok (Results.to_csv ~headers ~obs ~t ~contacts:(Some contacts))
            | `Csv_dist ->
                let normalize = not no_normalize in
                Ok (Results.stable_contact_distances_to_csv
                      ~normalize ~headers ~obs ~t ~contacts)
            | `Json_dist ->
                let normalize = not no_normalize in
                Result.ok @@ String.concat ",\n" @@
                Results.stable_contact_distances_to_json_objs
                  ~normalize ~obs ~t ~contacts
          in
          match res with
          | Error e ->
              Log.err (fun m -> m "%s : %s" (Observation.id obs) e);
              add_obs ~err:true ~headers:false acc obss
          | Ok res ->
          add_obs ~err ~headers:false (res :: acc) obss
      in
      let err, res =
        time begin fun (_, res) m ->
          m "Processed %d observations from %a" (List.length res) Fpath.pp dir
        end @@ fun () ->
        add_obs ~err:false ~headers:true [] obss in
      let res =
        if fmt = `Json_dist
        then String.concat "" ["[\n"; String.concat "," (List.rev res); "]\n"]
        else String.concat "" (List.rev res)
      in
      let* () = Os.File.write ~force:false ~make_path:true outf res in
      if err
      then (Log.warn (fun m -> m "Errors occured"); Ok 1)
      else Ok 0

let debug dir id scale min_max_distance (contact_spec : Cell.Contact.spec) =
  let iter_results tm cells contacts f =
    for i = 0 to Array.length cells - 1 do
      let cell = cells.(i) in
      let track =
        Option.get @@
        Trackmate.Int_map.find_opt cell.Cell.track_id tm.Trackmate.tracks_by_id
      in
      let contacts = contacts.(i) in
      f tm i cell track contacts
    done
  in
  Log.if_error ~use:1 @@
  let* dir = Fpath.of_string dir in
  let* obss = Cli_data.load_observations dir in
  let obs = List.hd obss in
  match Observation.target obs, Observation.t obs with
  | Some target_tm, Some t_tm ->
      Printf.printf "scale: %f\n" scale;
      Printf.printf "min-max-distance: %f\n" min_max_distance;
      Printf.printf "min-frame-count: %d\n" contact_spec.min_frame_count;
      Printf.printf "min-overlap: %d%%\n" contact_spec.min_overlap_pct;
      Printf.printf "allowed-gap: %d\n" contact_spec.allowed_overlap_gap_length;
      let* target = Cell.Group.of_trackmate target_tm in
      let* t = Cell.Group.of_trackmate ~scale ~min_max_distance t_tm in
      let* isects, err = Cell.Group.intersections t target in
      let contacts = Cell.Contact.find contact_spec ~t ~target ~isects in
      if err <> 0 then Printf.eprintf "Isect errors: %d" err;
      (iter_results t_tm t contacts @@ fun tm i c track contacts ->
       let print_result tm i c track contacts =
         let cms = Cell.mean_speed tm c in
         let tm_cms = track.Trackmate.track_mean_speed in
         let visited = Cell.Contact.unique_stable_count contacts in
         let ms_stbl = Cell.mean_speed_stable_contact tm c contacts in
         let _ms_tr = Cell.mean_speed_transient_contact tm c contacts in
         let ms_no = Cell.mean_speed_no_contact tm c contacts in
    (*     if ms_stbl <> 0. then *)
           Printf.printf "%03d visited:%d cms:%.5f %.5f stbl:%.5f no:%.5f \
                          strange:%b\n"
             c.track_id visited cms tm_cms ms_stbl ms_no (ms_stbl > ms_no)
       in
       match id with
       | None -> print_result tm i c track contacts
       | Some id when c.track_id = id -> print_result tm i c track contacts
       | Some _ -> ()
      );
      Ok 0
  | _ -> Error "Could not find t-cells and target cells in observation."

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let obs_dir =
  let doc = "Observation directory. The observation directory should \
             have matchgin $(b,*-t.xml) and $(b,*-target.xml) TrackMate \
             XML files."
  in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"OBSDIR")

let t_scale =
  let doc = "T cell scale factor." in
  Arg.(value & opt float Cell.Group.t_scale_default &
       info ["t-scale"] ~doc ~docv:"SCALE")

let t_min_max_distance =
  let doc =
    "T dead limit (um). Minimal maximal distance travelled to \
     keep a T cell."
  in
  Arg.(value & opt float Cell.Group.t_min_max_distance &
       info ["t-dead-limit"] ~doc ~docv:"DIST")

let contact_spec =
  let+ min_frame_count =
    let doc = "Min. frames for stable contact." in
    Arg.(value & opt int Cell.Contact.spec_default.min_frame_count &
         info ["c-min-frame-count"] ~doc ~docv:"FRAMECOUNT")
  and+ allowed_overlap_gap_length =
    let doc = "Allowed overlap gap length." in
    Arg.(value & opt int Cell.Contact.spec_default.allowed_overlap_gap_length &
         info ["c-allowed-gap"] ~doc ~docv:"FRAMECOUNT")
  and+ min_overlap_pct =
    let doc = "Minimal percentage overlap for contact." in
    Arg.(value & opt int Cell.Contact.spec_default.min_overlap_pct &
         info ["c-min-pct"] ~doc ~docv:"PCT")
  in
  { Cell.Contact.min_frame_count; allowed_overlap_gap_length; min_overlap_pct }

let results =
  let out_fmt =
    let fmts = [ "pdf", `Pdf; "csv", `Csv; "csv-dist", `Csv_dist;
                 "json-dist", `Json_dist; ] in
    let doc =
      Fmt.str "Output format. Must be %s." (Arg.doc_alts_enum fmts)
    in
    Arg.(value & opt (Arg.enum fmts) `Csv &
         info ["f"; "output-format"] ~doc ~docv:"FMT")
  in
  let outf =
    let doc = "Output file. Use $(b,-) for stdout." and docv = "FILE" in
    Arg.(value & opt B0_std_cli.fpath Fpath.dash & info ["o"] ~doc ~docv)
  in
  let no_isect =
    let doc = "Do not intersect." in
    Arg.(value & flag & info ["no-intersect"] ~doc)
  in
  let no_normalize =
    let doc = "Do not normalize contact distances." in
    Arg.(value & flag & info ["no-normalize"] ~doc)
  in
  Cmd.v (Cmd.info "results" ~doc:"Compute contact results") @@
  let+ out_fmt and+ obs_dir and+ outf and+ no_isect and+ t_scale
  and+ t_min_max_distance and+ contact_spec and+ no_normalize in
  results ~out_fmt ~obs_dir ~outf ~no_isect
    ~t_scale ~t_min_max_distance ~contact_spec ~no_normalize

let debug =
  let t_cell_id =
    let doc = "Cell id" in
    Arg.(value & opt (some int) None & info ["id"] ~doc ~docv:"ID")
  in
  Cmd.v (Cmd.info "debug") @@
  Term.(const debug $ obs_dir $ t_cell_id $
        t_scale $ t_min_max_distance $ contact_spec)

let cell =
  let doc = "Process trackmate cell data" in
  Cmd.group (Cmd.info "cell" ~version:"%%VERSION%%" ~doc) @@
  [ results; debug]

let main () = Cmd.eval' cell
let () = if !Sys.interactive then () else exit (main ())
