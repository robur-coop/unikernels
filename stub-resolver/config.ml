(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let resolver =
  let doc = Key.Arg.info ~doc:"Recursive resolver to query" ["resolver"] in
  Key.(create "resolver" Arg.(opt ipv4_address (Ipaddr.V4.of_string_exn "141.1.1.1") doc))

let dns_handler =
  let packages =
    [
      package "logs" ;
      package ~sublibs:["mirage"] "dns-resolver";
      package "dns-server";
    ]
  in
  foreign
    ~keys:[Key.abstract resolver]
    ~packages
    "Unikernel.Main" (random @-> pclock @-> mclock @-> time @-> stackv4 @-> job)

let () =
  register "stub-resolver" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4 default_network ]
