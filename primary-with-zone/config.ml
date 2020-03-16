(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let disk = generic_kv_ro "data"

let dns_handler =
  let packages =
    [
      package "logs" ;
      package ~sublibs:[ "zone" ; "mirage" ] "dns-server";
      package "dns-tsig";
      package ~min:"2.0.0" "mirage-kv";
    ]
  in
  foreign
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4 @-> kv_ro @-> job)

let () =
  register "primary"
    [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4 default_network $ disk ]
