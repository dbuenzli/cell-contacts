Cell contacts - Detect contacts between T and target cells
==========================================================

Processes paired [TrackMate] XML files to detect contacts between T
and target cells according to a few tweakable parameters.

[TrackMate]: https://imagej.net/plugins/trackmate/

# Building

A standalone build of the GUI app can be found in [app](app/) and can
be previewed [here].

To build from the sources you will need an [OCaml and `opam` install](https://ocaml.org/install).
Then proceed with:

```
opam pin b0 --dev-repo
opam install --deps-only .
b0 -- cell-contacts results data > results.csv
b0 -- cell-contacts --help  # For more info
b0 -- .show-url app         # For the GUI app
```

[here]: https://htmlpreview.github.io/?https://github.com/dbuenzli/cell-contacts/blob/main/app/cell-contacts.html

# Screenshots 

![screenshot](doc/screenshot.png)
