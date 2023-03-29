let set_share_dir, share_dir, binaries_dir, db_dir =
  let share_dir =
    let home_path = Unix.getenv "HOME" in
    ref (Filename.concat home_path ".local/share/benchtop")
  in
  ( (fun dir -> share_dir := dir),
    (fun () -> !share_dir),
    (fun () -> Filename.concat !share_dir "binaries"),
    (fun () -> Filename.concat !share_dir "databases") )

let set_tests_dir, tests_dir =
  let tests_dir =
    let home_path = Unix.getenv "HOME" in
    ref (Filename.concat home_path "tests")
  in
  ((fun dir -> tests_dir := dir),
   (fun () -> !tests_dir))

let set_number_of_jobs, number_of_jobs =
  let number_of_jobs = ref 60 in
  ((fun j -> number_of_jobs := j),
   (fun () -> !number_of_jobs))

let set_prover_timeout, prover_timeout =
  let prover_timeout = ref 20 in
  ((fun timeout -> prover_timeout := timeout),
   (fun () -> !prover_timeout))
