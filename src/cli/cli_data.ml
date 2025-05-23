(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let time fm f = Log.time ~level:Log.Info fm f

type pair = { t : Fpath.t option; target : Fpath.t option }

let add_t id file m =
  let upd = function
  | None -> Some { t = Some file; target = None }
  | Some p -> Some { p with t = Some file }
  in
  String.Map.update id upd m

let add_target id file m =
  let upd = function
  | None -> Some { t = None; target = Some file }
  | Some p -> Some { p with target = Some file }
  in
  String.Map.update id upd m

let find_file_pairs files =
  let warn file =
    Log.warn (fun m -> m "Don't know what to do with %a" Fpath.pp file)
  in
  let find acc file =
    if not (Fpath.has_ext ".xml" file) then (warn file; acc) else
    let fname = Fpath.basename file in
    match Observation.is_t_filename fname with
    | Some id -> add_t id file acc
    | None ->
        match Observation.is_target_filename fname with
        | Some id -> add_target id file acc
        | None -> warn file; acc
  in
  List.fold_left find String.Map.empty files

let find_observations_in_dir dir =
  let* files = Os.Dir.fold_files ~recurse:false Os.Dir.path_list dir [] in
  Ok (find_file_pairs files)

let read_trackmate ~kind file =
  time begin fun r m -> match r with
  | Ok (Some tm) -> m "Read trackmate %s data from %s" kind tm.Trackmate.file
  | _ -> r
  end @@ fun () -> match file with
  | None -> Ok None
  | Some file ->
      Result.map Option.some @@ Trackmate_xmlm.of_file (Fpath.to_string file)

let load_observations dir =
  time begin fun obs m -> match obs with
  | Ok obs ->
      m "Read %d observations from %a" (List.length obs) Fpath.pp dir
  | _ -> obs
  end @@ fun () ->
  let* m = find_observations_in_dir dir in
  let add id { t; target } acc  =
    let o =
      let* t = read_trackmate ~kind:"T" t in
      let* target = read_trackmate ~kind:"target" target in
      Observation.v ~id ~t ~target
    in
    match o with
    | Error e -> Log.err (fun m -> m "%s" e); acc
    | Ok o -> o :: acc
  in
  Ok (String.Map.fold add m [])
