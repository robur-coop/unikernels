(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let dns_handler =
  let packages = [
    package "logs" ;
    package ~sublibs:[ "server" ; "mirage.server" ; "crypto" ]
      ~pin:"git+https://github.com/roburio/udns.git" "udns" ;
    package "nocrypto"
  ] in
  foreign
    ~deps:[abstract nocrypto]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4 @-> job)

let () =
  register "primary" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4 default_network ]
