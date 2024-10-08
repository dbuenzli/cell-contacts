(*---------------------------------------------------------------------------
   Copyright (c) 2022 The brr programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr
open Brr_webworkers
open Brr_io

module type WORK = sig
  type 'a t
  val perform : 'a t -> 'a Fut.t
end

module type T = sig
  type 'a work
  type t
  val make : unit -> (t, Jv.Error.t) Fut.result
  val main : unit -> unit
  val send : t -> 'a work -> 'a Fut.t
end

module Make (Work : WORK) = struct
  type 'a work = 'a Work.t
  type setter = Set : ('a -> unit) -> setter
  type t = { w : Worker.t; results : setter Queue.t }

  let timer tag = let tag = Jstr.v tag in Console.time tag; tag

  let recv_result q e = match Queue.pop q.results with
  | Set set ->
      let tt = timer "Page: receiving result" in
      let d = Message.Ev.data (Ev.as_type e) in
      Console.time_end tt;
      set d

  let make () =
    (* The circonvolutions are needed to work over the file:// protocol. *)
    let open Fut.Result_syntax in
    let script = Jv.get (Document.to_jv G.document) "currentScript" in
    let script = Jv.to_jstr (Jv.get script "text") in
    let blob_init = Blob.init ~type':(Jstr.v "text/javascript") () in
    let blob = Blob.of_jstr ~init:blob_init script in
    let* url = Blob.data_uri blob in
    try
      let q = { w = Worker.create url; results = Queue.create () } in
      let target = Worker.as_target q.w in
      let () = ignore (Ev.listen Message.Ev.message (recv_result q) target) in
      Fut.ok q
    with Jv.Error e -> Fut.error e

  let send q work =
    let tt = timer "Page: sent work" in
    let f, set = Fut.create () in
    Queue.add (Set set) q.results;
    Worker.post q.w work;
    Console.time_end tt;
    f

  let recv_work e =
    let tt = timer "Worker: received work" in
    let w = (Message.Ev.data (Ev.as_type e) : 'a Work.t) in
    Console.time_end tt;
    let tt = timer "Worker: worked" in
    Fut.await (Work.perform w) (fun v ->
        Console.time_end tt;
        let tt = timer "Worker: sending result" in
        Worker.G.post v;
        Console.time_end tt)

  let main () = ignore (Ev.listen Message.Ev.message recv_work G.target)
end
