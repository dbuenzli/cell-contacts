(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open Gg

let ( let* ) = Result.bind

let time fm f = Log.time ~level:Log.App fm f

let intersections no_isect obs ~scale ~min_max_distance =
  let* target = match Observation.target obs with
  | None -> Ok None
  | Some target -> Result.map Option.some (Cell.Group.of_trackmate target)
  in
  let* t = match Observation.t obs with
  | None -> Ok None
  | Some t ->
      Result.map Option.some
        (Cell.Group.of_trackmate ~scale ~min_max_distance t)
  in
  let* isect =
    if no_isect then Ok None else match t, target with
    | Some t, Some target ->
        time (fun r m ->
            let errs = match r with
            | Ok None -> 0 | Ok (Some (_, e)) -> e | Error _ -> 1
            in
            m "Intersected groups (%d errors)" errs)
          (fun () ->
             (Result.map Option.some (Cell.Group.intersections t target)))
    | _ -> Ok None
  in
  Ok (t, target, isect)

let to_pdf ~dst obs target t isect =
  time (fun _ m -> m "Rendered %s" dst) @@ fun () ->
  match target, t with
  | None, None -> assert (false);
  | Some t, None | None, Some t ->
      let imgs = Cell_img.group t in
      Cell_img.render_pdf ~dst (Observation.ref obs) imgs
  | Some target, Some t ->
      let imgs = Cell_img.groups t target (Option.map fst isect) in
      Cell_img.render_pdf ~dst (Observation.ref obs) imgs

let data out_fmt dir outf no_isect scale min_max_distance =
  Log.if_error ~use:1 @@
  let* dir = Fpath.of_string dir in
  let* obs = Cli_data.load_observation dir in
  let* t, target, isect = intersections no_isect obs ~scale ~min_max_distance in
  match out_fmt with
  | `Pdf ->
      let dst = Fpath.to_string (Fpath.(dir / "tracking.pdf")) in
      let* () = to_pdf ~dst obs target t isect in Ok 0
  | `Csv -> (failwith "TODO" : unit); Ok 0

let debug dir scale min_max_distance contact_spec =
  Log.if_error ~use:1 @@
  let* dir = Fpath.of_string dir in
  let* obs = Cli_data.load_observation dir in
  match Observation.target obs, Observation.t obs with
  | Some target, Some t ->
      let* target = Cell.Group.of_trackmate target in
      let* t = Cell.Group.of_trackmate ~scale ~min_max_distance t in
      Ok 0
  | _ -> Error "Could not find t-cells and target cells in observation."

(* Command line interface *)
open Cmdliner

let obs_dir =
  let doc = "Observation directory. The observation directory should \
             have one $(b,t-*.xml) and one (b,target-*.xml) TrackMate \
             XML file."
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
  let min_frame_count =
    let doc = "Min. frames for stable contact." in
    Arg.(value & opt int Cell.Contact.spec_default.min_frame_count &
         info ["c-min-frame-count"] ~doc ~docv:"FRAMECOUNT")
  in
  let allowed_overlap_gap_length =
    let doc = "Allowed overlap gap length." in
    Arg.(value & opt int Cell.Contact.spec_default.allowed_overlap_gap_length &
         info ["c-allowed-gap"] ~doc ~docv:"FRAMECOUNT")
  in
  let min_overlap_pct =
    let doc = "Minimal percentage overlap for contact." in
    Arg.(value & opt int Cell.Contact.spec_default.min_overlap_pct &
         info ["c-min-pct"] ~doc ~docv:"PCT")
  in
  let make min_frame_count allowed_overlap_gap_length min_overlap_pct =
    { Cell.Contact.min_frame_count;
      allowed_overlap_gap_length; min_overlap_pct }
  in
  Term.(const make $ min_frame_count $ allowed_overlap_gap_length $
        min_overlap_pct)

let data =
  let out_fmt =
    let fmts = [ "pdf", `Pdf; "csv", `Csv] in
    let doc =
      Fmt.str "Output format. Must be %s." (Arg.doc_alts_enum fmts)
    in
    let docv = "FMT" in
    Arg.(value & opt (Arg.enum fmts) `Pdf &
         info ["f"; "output-format"] ~doc ~docv)
  in
  let outf =
    let doc = "Output file. Use $(b,-) for stdout." and docv = "FILE" in
    Arg.(value & opt string "-" & info ["o"] ~doc ~docv)
  in
  let no_isect =
    let doc = "Do not intersect." in
    Arg.(value & flag & info ["no-intersect"] ~doc)
  in
  Cmd.v (Cmd.info "data")
    Term.(const data $ out_fmt $ obs_dir $ outf $ no_isect $
          t_scale $ t_min_max_distance)

let debug =
  Cmd.v (Cmd.info "debug")
    Term.(const debug $ obs_dir $ t_scale $ t_min_max_distance $ contact_spec)

let cell =
  let doc = "Process trackmate cell data" in
  Cmd.group (Cmd.info "cell" ~version:"%%VERSION%%" ~doc) @@
  [ data; debug]

let main () = Cmd.eval' cell
let () = if !Sys.interactive then () else exit (main ())
