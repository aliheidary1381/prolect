(lang dune 3.17)

(name prolect)
(version 1.1.3)
(license "CC0-1.0")
(authors "Ali Heydari <aliheidary1381@gmail.com>")
(maintainers "Ali Heydari <aliheidary1381@gmail.com>")
(source (github aliheidary1381/prolect))
(documentation "https://github.com/aliheidary1381/prolect/blob/main/README.md")

(package
 (name prolect)
 (synopsis "Pure prolog interpreter")
 (description
"A pretty basic interpreter for prolog with no support for 
integers, lists or anything other than pure logic")
 (depends (ocaml (>= 5.1)) dune (linenoise (<> :os "win32")) ppx_optcomp)
 ; %{os_type} is a dune var, :os is an opam var
 (tags
  (prolog interpreter)))

(map_workspace_root false)
(generate_opam_files true)