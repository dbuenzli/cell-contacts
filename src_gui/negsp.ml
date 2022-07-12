(*---------------------------------------------------------------------------
   Copyright (c) 2022 The negsp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module A = struct
  type at = Brr.At.t
  let void = Brr.At.void
  let cl id = Brr.At.class' (Jstr.v id)
  let st s = Brr.At.style (Jstr.v s)
  let cat sep cs = String.concat sep cs
end

type at = A.at

let style_of_list ss = match A.cat ";" ss with
| "" -> A.void | style -> A.st style

let push_style name v st = match v with
| None -> st | Some v -> A.cat "" [name; v] :: st

module Size = struct
  type t = string
  let zero = "0"
  let thin = "1px"

  (* Space *)

  type space = [ `XXS | `XS | `S | `M | `L | `XL | `XXL | `XXXL | `XXXXL ]

  let space_to_string = function
  | `XXS -> "xxs" | `XS -> "xs" | `S -> "s" | `M -> "m" | `L -> "l"
  | `XL -> "xl" | `XXL -> "xxl" | `XXXL -> "xxxl" | `XXXXL -> "xxxxl"

  let space = function
  | `XXS -> "var(--sp_xxs)" | `XS -> "var(--sp_xs)"
  | `S -> "var(--sp_s)" | `M -> "var(--sp_m)"
  | `L -> "var(--sp_l)" | `XL -> "var(--sp_xl)"
  | `XXL -> "var(--sp_xxl)" | `XXXL -> "var(--sp_xxxl)"
  | `XXXXL -> "var(--sp_xxxxl)"

  let space_minmax min max =
    A.cat "-" ["--sp"; space_to_string min; space_to_string max]

  (* Gaps *)

  type gap = [ `Zero | `Sp of space | `Minmax of space * space | `Size of t ]

  let gap = function
  | `Zero -> zero
  | `Sp sp -> space sp
  | `Minmax (min, max) -> space_minmax min max
  | `Size s -> s

  (* Measure *)

  type measure = [ `XS | `S | `M | `L | `XL ]

  let measure = function
  | `XS -> "var(--measure_xs)"
  | `S -> "var(--measure_s)"
  | `M -> "var(--measure_m)"
  | `L -> "var(--measure_l)"
  | `XL -> "var(--measure_xl"

  (* Space *)

  let leading_m = "var(--leading_m)"
  let measure_m = "var(--measure_m)"
  let raw = Fun.id
end

module Text = struct
  let flow () = [A.cl "flow"]

  type size =
  [ `XXS | `XS | `S | `M | `L | `XL | `XXL | `Custom of Size.t * Size.t option]

  let size = function
  | `XXS -> A.st "font-size: var(--font_xxs)"
  | `XS -> A.st "font-size: var(--font_xs)"
  | `S -> A.st "font-size: var(--font_s)"
  | `M -> A.st "font-size: var(--font_m)"
  | `L -> A.st "font-size: var(--font_l)"
  | `XL -> A.st "font-size: var(--font_xl)"
  | `XXL -> A.st "font-size: var(--font_xxl)"
  | `Custom (size, None) -> A.st (A.cat "" ["font-size:"; size])
  | `Custom (size, (Some lh)) ->
      A.st (A.cat ";"
            [A.cat "" ["font-size:"; size];
             A.cat "" ["line-height:"; lh]])


  type align = [ `Start | `End | `Center | `Justify ]
  let align = function
  | `Start -> A.st "text-align:start"
  | `End -> A.st "text-align:end"
  | `Center -> A.st "text-align:center"
  | `Justify -> A.st "text-align:justify; hyphens:auto"

  type leading =
  [ `None | `XS | `S | `M | `L | `XL | `Custom of Size.t ]

  let leading = function
  | `None -> A.st "line-height: 1"
  | `XS -> A.st "line-height:var(--leading_ratio_xs)"
  | `S -> A.st "line-height:var(--leading_ratio_s)"
  | `M -> A.st "line-height:var(--leading_ratio_m)"
  | `L -> A.st "line-height:var(--leading_ratio_l)"
  | `XL -> A.st "line-height:var(--leading_ratio_xl)"
  | `Custom size -> A.st (A.cat "" ["line-height:"; size])
end

module Layout = struct
  type align = [ `Start | `End | `Center | `Baseline ]

  let align = function
  | `Start -> "start" | `End -> "end" | `Center -> "center"
  | `Baseline -> "baseline"

  type justify =
  [ `Start | `End | `Center | `Space_between | `Space_around
  | `Space_evenly | `Stretch ]

  let justify = function
  | `Start -> "start" | `End -> "end" | `Center -> "center"
  | `Space_between -> "space-between" | `Space_around -> "space-around"
  | `Space_evenly -> "space-evenly" | `Stretch -> "stretch"

  let stack ?gap () =
    let style = push_style "--stack_gap:" (Option.map Size.gap gap) [] in
    [A.cl "stack"; style_of_list style]

  let box ?border ?pad () =
    let style = push_style "--box_border:" border [] in
    let style = push_style "--box_pad:" (Option.map Size.gap pad) style in
    [A.cl "box"; style_of_list style]

  let center ?(children = true) ?pad ?max_size () =
    let style = if children then [] else ["--center_align:center"] in
    let style = push_style "--center_pad:" (Option.map Size.gap pad) style in
    let style = push_style "--center_max_size:" max_size style in
    [A.cl "center"; style_of_list style]

  let cluster ?gap ?align:a ?justify:j () =
    let style = push_style "--cluster_gap:" (Option.map Size.gap gap) [] in
    let style = push_style "--cluster_align:" (Option.map align a) style in
    let style = push_style "--cluster_justify:" (Option.map justify j) style in
    [A.cl "cluster"; style_of_list style]

  let sidebar ?gap ?bar_size ?min_content bar_pos () =
    let style = push_style "--sidebar_gap:" (Option.map Size.gap gap) [] in
    let style = push_style "--sidebar_bar_size" bar_size style in
    let style = match min_content with
    | None -> style
    | Some p ->
        A.cat "" ["--sidebar_min_content:"; string_of_int p; "%"] :: style
    in
    let pos = match bar_pos with `Start -> "start" | `End -> "end" in
    [A.cl "sidebar"; A.cl pos; style_of_list style]

  let switcher ?gap ?at_size () =
    let style = push_style "--switcher_gap:" (Option.map Size.gap gap) [] in
    let style = push_style "--switcher_at_size:" at_size style in
    [A.cl "switcher"; style_of_list style]

  let cover ?gap ?min_size () =
    let style = push_style "--cover_gap:" (Option.map Size.gap gap) [] in
    let style = push_style "--cover_min_size:" min_size style in
    [A.cl "cover"; style_of_list style]

  let central = A.cl "central"

  let grid ?gap ?min () =
    let style = push_style "--grid_gap:" (Option.map Size.gap gap) [] in
    let style = push_style "--grid_min:" min style in
    [A.cl "grid"; style_of_list style]

  let frame ?aspect () =
    let style = match aspect with
    | None -> []
    | Some (w, h) ->
        push_style "--frame_ratio_num:" (Some (string_of_int w)) @@
        push_style "--frame_ratio_den:" (Some (string_of_int h)) []
    in
    [A.cl "frame"; style_of_list style]

  let reel ?scrollbar_size ?scrollbar_visible:v ?gap ?item_size () =
    let style = push_style "--reel_scrollbar_size:" scrollbar_size [] in
    let style =
      let vis = function `Auto -> "auto" | `Always -> "scroll" in
      push_style "--reel_scrollbar_visible:" (Option.map vis v) style
    in
    let style = push_style "--reel_gap:" (Option.map Size.gap gap) style in
    let style = push_style "--reel_item_size:" item_size style in
    [A.cl "reel"; style_of_list style]

  let imposter ?position:p ?(contained = true) ?pad () =
    let style = push_style "--imposter_pad:" (Option.map Size.gap pad) [] in
    let style =
      let pos = function `Ancestor -> "absolute" | `Viewport -> "fixed" in
      push_style "--imposter_position:" (Option.map pos p) style
    in
    let contained = if contained then A.cl "contained" else A.void in
    [A.cl "imposter"; contained; style_of_list style]

  let icon = A.cl "icon"
  let with_icon ?gap () =
    let style = push_style "--icon_gap:" (Option.map Size.gap gap) [] in
    [A.cl "with_icon"; style_of_list style]
end

module Font = struct

  (* Some of the style sets are borderline maybe some could benefit of
     CSS utility. *)

  type family = [ `Headings | `Body | `Mono | `Custom of string ]

  let family = function
  | `Headings -> A.st "font-family:var(--font_headings)"
  | `Body -> A.st "font-family:var(--font_body)"
  | `Mono -> A.st "font-family:var(--font_mono)"
  | `Custom f -> A.st (A.cat "" ["font-family:"; f])

  type slant = [ `Normal | `Italic | `Oblique | `Custom of string ]

  let slant = function
  | `Normal -> A.st "font-style:normal"
  | `Italic -> A.st "font-style:italic"
  | `Oblique -> A.st "font-style:oblique"
  | `Custom s -> A.st (A.cat "" ["font-style:"; s])

  type weight =
  [ `W100 | `W200 | `W300 | `W400 | `W500 | `W600 | `W700 | `W800 | `W900 ]

  let weight = function
  | `W100 -> A.st "font-weight:100" | `W200 -> A.st "font-weight:200"
  | `W300 -> A.st "font-weight:300" | `W400 -> A.st "font-weight:400"
  | `W500 -> A.st "font-weight:500" | `W600 -> A.st "font-weight:600"
  | `W700 -> A.st "font-weight:700" | `W800 -> A.st "font-weight:800"
  | `W900 -> A.st "font-weight:900"

  type stretch = [ `XXS | `XS | `S | `M | `L | `XL | `XXL]

  let stretch = function
  | `XXS -> A.cl ".font_stretch_xxs"
  | `XS -> A.cl ".font_stretch_xs"
  | `S -> A.cl ".font_stretch_s"
  | `M -> A.cl ".font_stretch_m"
  | `L -> A.cl ".font_stretch_l"
  | `XL -> A.cl ".font_stretch_xl"
  | `XXL -> A.cl ".font_stretch_xxl"

  type numerics = [ `Default | `Normal | `Tabular | `Custom of string ]

  let numerics = function
  | `Default -> A.st "font-variant-numeric:var(--font_numeric_default)"
  | `Normal -> A.st "font-variant-numeric:normal"
  | `Tabular -> A.st "font-variant-numeric:var(--font_numeric_tabular)"
  | `Custom c -> A.st (A.cat "" ["font-variant-numeric:"; c])
end

module Border = struct
  type style =
  [ `None
  | `Solid
  | `Custom of string ]

  let style = function
  | `None -> A.st "border-style:none"
  | `Solid -> A.st "border-style:solid"
  | `Custom c -> A.st (A.cat "" ["border-style:"; c])

  let width s = A.st (A.cat "" ["border-width:"; s])
  let block_start s = A.st (A.cat "" ["border-block-start-width:"; s])
  let block_end s = A.st (A.cat "" ["border-block-end-width:"; s])
  let inline_start s = A.st (A.cat "" ["border-inline-start-width:"; s])
  let inline_end s = A.st (A.cat "" ["border-inline-end-width:"; s])
end

module Sample = struct
  let text =
    "Oh… this and that I suppose, nothing in particular. Yes, now I \
     remember, yesterday evening we spent blathering about nothing in \
     particular. That's been going on now for half a century."

  let title = "A vague supplication"
end

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The negsp programmers

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
