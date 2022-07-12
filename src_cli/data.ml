(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let time fm f = Log.time ~level:Log.App fm f

let find_observation_files dir =
  let find _ fname file (t, target as acc) =
    if not (Fpath.has_ext ".xml" file) then acc else
    let fname = String.Ascii.lowercase fname in
    if Observation.is_t_filename fname then (Some file, target) else
    if Observation.is_target_filename fname then (t, Some file) else
    acc
  in
  Os.Dir.fold_files ~recurse:false find dir (None, None)

let read_trackmate ~kind file =
  time (fun r m -> match r with
    | Ok (Some tm) -> m "Read trackmate %s data from %s" kind tm.Trackmate.file
    | _ -> r)
  @@ fun () -> match file with
  | None -> Ok None
  | Some file ->
      Result.map Option.some @@ Trackmate_xmlm.of_file (Fpath.to_string file)

let load_observation dir =
  let* t, target = find_observation_files dir in
  let* t = read_trackmate ~kind:"T" t in
  let* target = read_trackmate ~kind:"target" target in
  Observation.v ~t ~target

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
