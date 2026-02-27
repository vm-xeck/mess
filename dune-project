(lang dune 3.15)

(name mess)

(generate_opam_files true)

(source
 (github vm-xeck/mess))

(authors "Nori Sens")

(license "Unlicense")

(package
 (name mess)
 (synopsis "pager")
 (description "p a g e r")
 (depends ocaml dune curses))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
