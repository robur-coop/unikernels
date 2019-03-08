(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let dns_handler =
  let packages =
    let pin = "git+https://github.com/roburio/udns.git" in
    [
      package "logs" ;
      package ~pin "udns-server";
      package ~pin "udns-mirage-resolver";
      package ~pin "udns-tsig";
      package "randomconv" ;
      package "lru" ;
      package "rresult" ;
      package "duration" ;
    ]
  in
  foreign
    ~deps:[abstract nocrypto ; abstract app_info]
    ~packages
    "Unikernel.Main" (random @-> pclock @-> mclock @-> time @-> stackv4 @-> job)

let () =
  register "stub-resolver" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4 default_network ]
