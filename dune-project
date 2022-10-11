(lang dune 3.4)

(name benchtop)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Pierre Villemot")
(maintainers "Pierre Villemot")

(using dune_site 0.1)

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name benchtop)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   dune-site
   (lwt (>= 5.0))
   (inotify (>= 2.0))
   dream
   (tyxml-ppx (>= 4.0.0))
   (caqti-driver-sqlite3 (>= 1.9.0))
   
 )
 (tags
  (topics "to describe" your project))
 (sites
   (share tests)
   (share configs)
   (bin css)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
