open B0_kit.V000

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let fmt = B0_ocaml.libname "fmt"
let xmlm = B0_ocaml.libname "xmlm"
let gg = B0_ocaml.libname "gg"
let gg_kit = B0_ocaml.libname "gg.kit"
let vg = B0_ocaml.libname "vg"
let vg_pdf = B0_ocaml.libname "vg.pdf"
let vg_htmlc = B0_ocaml.libname "vg.htmlc"
let brr = B0_ocaml.libname "brr"
let brr_poked = B0_ocaml.libname "brr.poked"
let note = B0_ocaml.libname "note"
let note_brr = B0_ocaml.libname "note.brr"
let evidence = B0_ocaml.libname "vz.evidence"

let cmdliner = B0_ocaml.libname "cmdliner"

(* let vz_doc = B0_ocaml.libname "vz.doc" *)

let show_uri_action f build u ~args =
  (* FIXME get that into b0 *)
  let open Result.Syntax in
  Fut.return @@ Log.if_error ~use:B0_cli.Exit.some_error @@
  let url = Fpath.(B0_build.build_dir build u / f) in
  let background = true and prefix = true and
    url = Fmt.str "file://%s" (Fpath.to_string url)
  in
  let* b = B0_web_browser.find ~browser:None () in
  let* () = B0_web_browser.show ~background ~prefix b url in
  Ok B0_cli.Exit.ok

let vcs_describe b =
  (* XXX memo ? *)
  let open Result.Syntax in
  let dir = B0_build.scope_dir b (B0_build.current b) in
  let* vcs = B0_vcs.get () ~dir in
  B0_vcs.describe vcs ~dirty_mark:true "HEAD"

let favicon = Fpath.v "src_gui/assets/favicon.ico"
let font = Fpath.v "src_gui/assets/Inter.var.woff2"
let negsp_css = Fpath.v "src_gui/negsp.css"
let gui_css = Fpath.v "src_gui/gui.css"
let linker = Fpath.v "src_gui/gui.html"

let data_url b ~type' file = (* XXX add something like that to b0 *)
  let open Fut.Syntax in
  let* favicon = B0_memo.read (B0_build.memo b) file in
  let b64 = B0_base64.encode favicon in
  Fut.return (String.concat "" ["data:"; type'; ";base64,"; b64])

let build_cell_gui proc b =
  let open Fut.Syntax in
  ignore (proc b);
  let m = B0_build.memo b in
  let favicon = B0_build.in_scope_dir b favicon in
  let font = B0_build.in_scope_dir b font in
  let gui_css = B0_build.in_scope_dir b gui_css in
  let negsp_css = B0_build.in_scope_dir b negsp_css in
  let linker = B0_build.in_scope_dir b linker in
  let script = B0_build.in_build_dir b (Fpath.v "gui.js") in
  let app = B0_build.in_build_dir b (Fpath.v "cell.html") in
  let reads = [favicon; font; negsp_css; gui_css; linker; script] in
  let version = vcs_describe b |> B0_memo.fail_if_error m in
  List.iter
    (B0_memo.file_ready m) [favicon;font; gui_css; negsp_css; linker];
  begin B0_memo.write m ~reads ~stamp:version app @@ fun () ->
    Fut.sync @@
    let* linker = B0_memo.read m linker in
    let* script = B0_memo.read m script in
    let* gui_css = B0_memo.read m gui_css in
    let* negsp_css = B0_memo.read m negsp_css in
    let css = String.concat "\n" [negsp_css; gui_css] in
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

let protocol =
  let exe_proc b =
    let open Fut.Syntax in
    let m = B0_build.memo b in
    let scope = B0_build.current_scope_dir b in
    let md = Fpath.(scope / "PROTOCOL.md") in
    let md_html = Fpath.(scope / "doc" / "md.html") in
    let md_css = Fpath.(scope / "doc" / "md.css") in
    let build = B0_build.current_build_dir b in
    let html = Fpath.(build / "PROTOCOL.html") in
    let hfrag = Fpath.(build / "PROTOCOL.hfrag") in
    List.iter (B0_memo.file_ready m) [md; md_html; md_css];
    B0_cmark.cmd m ~opts:Cmd.(arg "--unsafe") ~mds:[md] ~o:hfrag;
    begin B0_memo.write m ~reads:[hfrag; md_html; md_css] html @@ fun () ->
      Fut.sync @@
      let* md_html = B0_memo.read m md_html in
      let* md_css = B0_memo.read m md_css in
      let* frag = B0_memo.read m hfrag in
      let vars = function
      | "BODY" -> Some frag | "CSS" -> Some md_css | _ -> None
      in
      let content = String.subst_pct_vars vars md_html in
      Fut.return (Ok (content));
    end;
    Fut.return ()
  in
  let action = show_uri_action "PROTOCOL.html" in
  B0_unit.v "protocol" ~doc:"Description of the protocol" ~action exe_proc

let cell_gui =
  let srcs = [`Dir (Fpath.v "src"); `Dir (Fpath.v "src_gui")] in
  let requires =
    [fmt; gg; gg_kit; vg; vg_pdf; vg_htmlc; brr; note; note_brr; evidence]
  in
  let comp_mode = `Whole and source_map = None (* Some `Inline *) in
  let meta =
    (* XXX I think should be no longer needed with jsoo 5. *)
    let comp = Cmd.(arg "--enable=use-js-string") in
    let link = comp in
    B0_jsoo.meta ~requires ~link ~comp ~comp_mode ~source_map ()
  in
  let action = show_uri_action "cell.html" and wrap = build_cell_gui in
  let doc = "Cell app" in
  B0_jsoo.exe ~action ~wrap "gui.js" ~name:"cell_gui" ~doc ~srcs ~meta

let cell_exe =
  let srcs = [`Dir (Fpath.v "src"); `Dir_rec (Fpath.v "src_cli");] in
  let requires = [b0_std; fmt; gg; gg_kit; vg; vg_pdf; cmdliner; xmlm] in
  B0_ocaml.exe "cell" ~srcs ~requires

let mount_dir = Fpath.v "deploy"
let webdav_url = "https://cloud.uni-konstanz.de/public.php/webdav"
let mount =
  let open Result.Syntax in
  B0_cmdlet.v "mount" ~doc:"(Un)mount deploy directory" @@ fun env args ->
  let doc = "Unmount deploy directory." in
  let unmount = Cmdliner.Arg.(value & flag & info ["u"; "unmount"] ~doc) in
  let mount u d = Os.Cmd.run Cmd.(arg "mount_webdav" % "-i" % u %% path d) in
  let umount d = Os.Cmd.run Cmd.(arg "umount" %% path d) in
  let mount unmount =
    B0_cmdlet.exit_of_result @@
    let dir = Fpath.(B0_cmdlet.Env.scope_dir env // mount_dir) in
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
  B0_cmdlet.eval env args Cmdliner.Term.(const mount $ unmount)


let deploy =
  let open Result.Syntax in
  B0_cmdlet.v "deploy" ~doc:"Deploy to mount directory" @@ fun env args ->
  B0_cmdlet.exit_of_result @@
  let dir = Fpath.(B0_cmdlet.Env.scope_dir env // mount_dir) in
  let mount_error = Error "No deploy directory use 'b0 -- mount' first" in
  let* exists = Os.Dir.exists dir in
  if not exists then mount_error else
  let* is_mount = Os.Path.is_mount_point dir in
  if not is_mount then mount_error else
  let scope = B0_cmdlet.Env.scope_dir env in
  let* () = Os.Cmd.run ~cwd:scope (Cmd.arg "b0") (* FIXME b0 *) in
  let build = Fpath.(B0_cmdlet.Env.b0_dir env / "b" / "user") in
  let protocol_md = Fpath.(B0_cmdlet.Env.scope_dir env / "PROTOCOL.md") in
  let protocol_html = Fpath.(build / "protocol" / "PROTOCOL.html") in
  let changes_md = Fpath.(B0_cmdlet.Env.scope_dir env / "CHANGES.md") in
  let cell_html = Fpath.(build / "cell_gui"/ "cell.html") in
  let copy f dir =
    Os.Cmd.run Cmd.(arg "cp" %% path f %%path Fpath.(dir / basename f))
(*
    Os.File.copy
      ~atomic:false (* This trips webdav *)
      ~force:true ~make_path:false ~src:f Fpath.(dir / basename f) *)
  in
  let* () = copy protocol_md dir in
  let* () = copy protocol_html dir in
  let* () = copy changes_md dir in
  let* () = copy cell_html dir in
  Ok ()



(* Default pack *)

(*
let default =
  B0_pack.v "default" ~locked:false ~doc:"Cell tracker app" [cell]
*)
