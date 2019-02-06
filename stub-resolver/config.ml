(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let dns_handler =
  let packages = [
    package "logs" ;
    package ~sublibs:[ "server" ; "mirage.resolver" ; "crypto" ]
      ~pin:"git+https://github.com/roburio/udns.git" "udns" ;
    package "randomconv" ;
    package "lru" ;
    package "rresult" ;
    package "duration" ;
  ] in
  foreign
    ~deps:[abstract nocrypto ; abstract app_info]
    ~packages
    "Unikernel.Main" (random @-> pclock @-> mclock @-> time @-> stackv4 @-> job)

let () =
  register "stub-resolver" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4 default_network ]
