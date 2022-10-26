let benchpress_share_dir = 
  let home_path = Unix.getenv "HOME" in
  Filename.concat home_path ".local/share/benchpress"

let binaries_dir = 
  let home_path = Unix.getenv "HOME" in
  Filename.concat home_path "binaries"

let tests_dir = 
  let home_path = Unix.getenv "HOME" in
  Filename.concat home_path "tests"

let configs_dir = List.hd Location.Sites.configs

let number_of_jobs = 60
let prover_timeout = 30
