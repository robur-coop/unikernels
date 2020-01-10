(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let axfr =
  let doc = Key.Arg.info ~doc:"Allow unauthenticated zone transfer." ["axfr"] in
  Key.(create "axfr" Arg.(flag doc))

let dns_handler =
  let packages =
    [
      package "logs" ;
      package ~min:"4.3.0" ~sublibs:[ "mirage" ] "dns-server";
      package "dns-tsig";
      package "nocrypto"
    ]
  in
  foreign
    ~keys:[Key.abstract axfr]
    ~deps:[abstract nocrypto]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4 @-> job)

let () =
  register "primary" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4 default_network ]
