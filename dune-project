(lang dune 3.4)

(name benchtop)

(generate_opam_files true)

(source
 (github OCamlPro/benchtop))

(authors "Pierre Villemot")
(maintainers "Pierre Villemot")

(using dune_site 0.1)

(documentation https://url/to/documentation)

(package
 (name benchtop)
 (synopsis "A web interface for benchpress")
 (description "A longer description")
 (depends
   ocaml
   dune
   dune-site
   cmdliner
   fmt
   (lwt (>= 5.0))
   (inotify (>= 2.0))
   dream
   (tyxml-ppx (>= 4.0.0))
   caqti
   caqti-lwt
   (caqti-driver-sqlite3 (>= 1.9.0))
   ppx_rapper_lwt
   camlzip
   fileutils
   benchpress)
 (tags
  (topics "to describe" your project))
 (sites
   (bin css)
   (bin scripts)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
