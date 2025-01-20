(lang dune 3.17)

(name prolect)
(license CC0)
(authors "Ali Heydari <aliheidary1381@gmail.com>")
(maintainers "Ali Heydari <aliheidary1381@gmail.com>")
(source (github aliheidary1381/prolect))
(documentation "https://github.com/aliheidary1381/prolect/blob/main/README.md")

(package
 (name prolect)
 (synopsis "pure prolog interpreter")
 (description
"A pretty basic interpreter for prolog with no support for 
integers, lists or anything other than pure logic")
 (depends ocaml dune)
 (tags
  (prolog interpreter)))
