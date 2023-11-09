(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
