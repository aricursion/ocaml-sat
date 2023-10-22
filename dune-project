(lang dune 3.11)

(name ocaml_sat)

(generate_opam_files true)

(source
 (github zaxioms/ocaml_sat))



(package
 (name ocaml_sat)
 (synopsis "A SAT library for Ocaml")
 (description "A library for interfacing with SAT solvers as well as CNF Encodings")
 (depends ocaml dune base stdio)
 (tags
  ("SAT" "SAT Solver")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
