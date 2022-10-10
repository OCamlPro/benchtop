let benchpress_share_dir = 
  let home_path = Unix.getenv "HOME" in
  Filename.concat home_path ".local/share/benchpress"

let configs_dir = List.hd Location.Sites.configs
let tests_dir = List.hd Location.Sites.tests


