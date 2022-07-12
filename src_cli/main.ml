(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open Gg

let ( let* ) = Result.bind

let time fm f = Log.time ~level:Log.App fm f

let intersections no_isect obs ~t_scale:scale =
  let make_group = function
  | None -> Ok None | Some t ->
      Result.map Option.some (Cell.Group.of_trackmate ?scale t)
  in
  let* t = make_group (Observation.t obs) in
  let* target = make_group (Observation.target obs) in
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

let cell out_fmt dir outf no_isect t_scale  =
  Log.if_error ~use:1 @@
  let* dir = Fpath.of_string dir in
  let* obs = Data.load_observation dir in
  let* t, target, isect = intersections no_isect obs ~t_scale in
  match out_fmt with
  | `Pdf ->
      let dst = Fpath.to_string (Fpath.(dir / "tracking.pdf")) in
      let* () = to_pdf ~dst obs target t isect in Ok 0
  | `Csv -> (failwith "TODO" : unit); Ok 0

(* Command line interface *)
open Cmdliner

let cell_cmd =
  let out_fmt =
    let fmts = [ "pdf", `Pdf; "csv", `Csv] in
    let doc =
      Fmt.str "Output format. Must be %s." (Arg.doc_alts_enum fmts)
    in
    let docv = "FMT" in
    Arg.(value & opt (Arg.enum fmts) `Pdf &
         info ["f"; "output-format"] ~doc ~docv)
  in
  let obs_dir =
    let doc = "Observation directory. The observation directory should \
               have one $(b,t-*.xml) and one (b,target-*.xml) TrackMate \
               XML file."
    in
    Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"OBSDIR")
  in
  let outf =
    let doc = "Output file. Use $(b,-) for stdout." and docv = "FILE" in
    Arg.(value & opt string "-" & info ["o"] ~doc ~docv)
  in
  let no_isect =
    let doc = "Do not intersect." in
    Arg.(value & flag & info ["no-intersect"] ~doc)
  in
  let t_scale =
    let doc = "T cell scale factor." in
    Arg.(value & opt (some float) None & info ["t-scale"] ~doc ~docv:"SCALE")
  in
  Cmd.v (Cmd.info "cell")
    Term.(const cell $ out_fmt $ obs_dir $ outf $ no_isect $ t_scale)

let () = if !Sys.interactive then () else exit (Cmd.eval' cell_cmd)


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
