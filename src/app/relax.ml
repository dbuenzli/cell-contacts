(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let run f = Fut.bind (Fut.tick ~ms:0) (fun () -> Fut.return (f ()))
let run' f = Fut.bind (Fut.tick ~ms:0) (fun () -> f ())

open Note

(* FIXME we need something like this but this has gc problems
   (cf. Logr.hold) *)

module E = struct
  let map f e =
    let r, send_r = E.create () in
    match E.log e (fun e -> ignore (Fut.map send_r (f e))) with
    | None -> E.never
    | Some log -> Logr.hold log; r
end

module S = struct

  (* Would be nice to use (S.value s) for init but then the map
     is in a fut. *)

  let map ?eq f s ~init =
    let runner, set_run = S.create ?eq init in
    let log v = ignore (run' (fun () -> Fut.map set_run (f v))) in
    Logr.hold (S.log s log);
    runner

  let patience_map ?eq ~filler f s =
    let runner, send_run = Note.E.create () in
    let log v = ignore (run' (fun () -> Fut.map send_run (f v))) in
    Logr.hold (S.log ~now:true s log);
    let fill = Note.E.map filler (S.changes s) in
    let map = Note.E.select [fill; runner] in
    let init = filler (S.value s) in
    S.hold ?eq init map
end
