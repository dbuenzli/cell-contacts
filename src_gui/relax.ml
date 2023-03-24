(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
