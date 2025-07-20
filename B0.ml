open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let fmt = B0_ocaml.libname "fmt"
let brr = B0_ocaml.libname "brr"
let cmdliner = B0_ocaml.libname "cmdliner"
let gg = B0_ocaml.libname "gg"
let gg_kit = B0_ocaml.libname "gg.kit"
let negsp_brr = B0_ocaml.libname "negsp.brr"
let note = B0_ocaml.libname "note"
let note_brr = B0_ocaml.libname "note.brr"
let vg = B0_ocaml.libname "vg"
let vg_htmlc = B0_ocaml.libname "vg.htmlc"
let vg_pdf = B0_ocaml.libname "vg.pdf"
let xmlm = B0_ocaml.libname "xmlm"
(* let evidence = B0_ocaml.libname "vz.evidence" *)
(* let vz_doc = B0_ocaml.libname "vz.doc" *)

(* Cli tool *)

let cell_contacts_cli =
  let doc = "Cell contacts batch processing" in
  let srcs = [`Dir ~/"src"; `Dir_rec ~/"src/cli";] in
  let requires = [b0_std; fmt; gg; gg_kit; vg; vg_pdf; cmdliner; xmlm] in
  B0_ocaml.exe "cell-contacts" ~doc ~srcs ~requires

(* HTML GUI *)

let vcs_describe b =
  (* TODO memo ? *)
  let open Result.Syntax in
  let dir = B0_build.scope_dir b in
  let* vcs = B0_vcs_repo.get () ~dir in
  B0_vcs_repo.describe vcs ~dirty_mark:true "HEAD"

let favicon = ~/"src/app/assets/favicon.ico"
let font = ~/"src/app/assets/Inter.var.woff2"
let negsp_css = ~/"src/app/negsp.css"
let app_css = ~/"src/app/app.css"
let linker = ~/"src/app/app.html"
let app_file = ~/"cell-contacts.html"

let data_url b ~type' file = (* TODO add something like that to b0 *)
  let open Fut.Syntax in
  let* favicon = B0_memo.read (B0_build.memo b) file in
  let b64 = B0_base64.encode `Padded favicon in
  Fut.return (String.concat "" ["data:"; type'; ";base64,"; b64])

let pack_app proc b =
  let open Fut.Syntax in
  ignore (proc b);
  (* Pack everything in the HTML file. *)
  let m = B0_build.memo b in
  let favicon = B0_build.in_scope_dir b favicon in
  let font = B0_build.in_scope_dir b font in
  let app_css = B0_build.in_scope_dir b app_css in
  let negsp_css = B0_build.in_scope_dir b negsp_css in
  let linker = B0_build.in_scope_dir b linker in
  let script = B0_build.in_current_dir b ~/"app.js" in
  let app_file = B0_build.in_current_dir b app_file in
  let reads = [favicon; font; negsp_css; app_css; linker; script] in
  let version = vcs_describe b |> B0_memo.fail_if_error m in
  B0_memo.ready_files m [favicon; font; app_css; negsp_css; linker];
  begin B0_memo.write m ~reads ~stamp:version app_file @@ fun () ->
    Fut.sync @@
    let* linker = B0_memo.read m linker in
    let* script = B0_memo.read m script in
    let* app_css = B0_memo.read m app_css in
    let* negsp_css = B0_memo.read m negsp_css in
    let css = String.concat "\n" [negsp_css; app_css] in
    let* font = data_url b ~type':"font/woff2" font in
    let font_var = function "FONT" -> Some font | _ -> None in
    let* favicon = data_url b ~type':"image/x-icon" favicon in
    let vars = function
    | "SCRIPT" -> Some script
    | "VERSION" -> Some version
    | "CSS" -> Some (String.subst_pct_vars font_var css)
    | "FAVICON" -> Some favicon
    | _ -> None
    in
    let content = String.subst_pct_vars vars linker in
    Fut.return (Ok content)
  end;
  Fut.return ()

let cell_contacts_app =
  let doc = "Cell contacts application" in
  let srcs = [`Dir ~/"src"; `Dir ~/"src/app"] in
  let requires =
    [fmt; gg; gg_kit; vg; vg_pdf; vg_htmlc; brr; note; note_brr; negsp_brr]
  in
  let meta =
    B0_meta.empty
    |> ~~ B0_jsoo.compilation_mode `Whole
    |> ~~ B0_jsoo.source_map None (* Some `Inline) *)
    |> ~~ B0_show_url.url (`In (`Unit_dir, app_file))
  in
  let wrap = pack_app in
  B0_jsoo.exe "app.js" ~name:"app" ~requires ~meta ~srcs ~wrap ~doc

(* Actions *)

let mount_dir = ~/"nosync/deploy"
let webdav_url = "https://cloud.uni-konstanz.de/public.php/webdav"
let mount =
  B0_unit.of_action' "mount" ~doc:"(Un)mount deploy directory" @@
  B0_unit.Action.of_cmdliner_term @@ fun env u ->
  let doc = "Unmount deploy directory." in
  let unmount = Cmdliner.Arg.(value & flag & info ["u"; "unmount"] ~doc) in
  let mount u d = Os.Cmd.run Cmd.(arg "mount_webdav" % "-i" % u %% path d) in
  let umount d = Os.Cmd.run Cmd.(arg "umount" %% path d) in
  let mount unmount =
    Os.Exit.of_result @@
    let dir = Fpath.(B0_env.scope_dir env // mount_dir) in
    match unmount with
    | true ->
        let* exists = Os.Dir.exists dir in
        if not exists then Ok () else
        let* is_mount = Os.Path.is_mount_point dir in
        let* () = if is_mount then umount dir else Ok () in
        let* _exists = Os.Path.delete ~recurse:false dir in
        Ok ()
    | false ->
        let* _created = Os.Dir.create ~make_path:true dir in
        let* is_mount = Os.Path.is_mount_point dir in
        if is_mount then Ok () else mount webdav_url dir
  in
  Cmdliner.Term.(const mount $ unmount)

let deploy =
  let doc = "Build and deploy to mount directory" in
  B0_unit.of_action "deploy" ~units:[cell_contacts_app] ~doc @@
  fun env _ ~args ->
  let dir = Fpath.(B0_env.scope_dir env // mount_dir) in
  let mount_error = Error "No deploy directory use 'b0 -- mount' first" in
  let* exists = Os.Dir.exists dir in
  if not exists then mount_error else
  let* is_mount = Os.Path.is_mount_point dir in
  if not is_mount then mount_error else
  let files =
    [ B0_env.in_scope_dir env ~/"PROTOCOL.md";
      B0_env.in_scope_dir env ~/"CHANGES.md";
      B0_env.in_scope_dir env ~/"PAPER.md";
      B0_env.in_unit_dir env cell_contacts_app app_file ]
  in
  let copy ~dst:dir file =
    Os.Cmd.run Cmd.(arg "cp" %% path file %%path Fpath.(dir / basename file))
(*
    Os.File.copy
      ~atomic:false (* This trips webdav *)
      ~force:true ~make_path:false ~src:f Fpath.(dir / basename f) *)
  in
  let* () = List.iter_stop_on_error (copy ~dst:dir) files in
  Ok ()

(* Default pack *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The cell contacts programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://github.com/dbuenzli/cell-contacts"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/cell-contacts/issues"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_opam.depends
      [ "b0", {|dev|};
        "ocaml", {|>= "5.1.0"|};
        "ocamlfind", {|build|};
        "fmt", {|>= "0.9.0"|};
        "brr", {|>= "0.0.7"|};
        "cmdliner", {|>= "1.3.0"|};
        "gg", {||};
        "vg", {||};
        "negsp", {||};
        "xmlm", {||};
        "note", {||};
        "js_of_ocaml", {|>= "5.1.0"|};
      ]
    |> ~~ B0_opam.pin_depends
      [ "negsp.dev", "git+https://erratique.ch/repos/negsp.git#main";
        "gg.dev", "git+https://erratique.ch/repos/gg.git#master"; ]
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~locked:false ~doc:"Cell contacts app" ~meta @@
  B0_unit.list ()
