(prover
  (name ae-read-status)
  (cmd "grep :status $file")
  (unknown ":status unknown")
  (sat ":status sat")
  (unsat ":status valid"))

(dir
  (path "lib/tests")
  (pattern ".*.ae|.*.smt2")
  (expect (run ae-read-status)))

(prover
  (name alt-ergo)
  (cmd "alt-ergo $file")
  (sat "^sat")
  (unsat "Valid|(^unsat)")
  (unknown "(I Don't Know)|(^unsat)"))
