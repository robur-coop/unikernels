(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let hostname =
  let doc = Key.Arg.info ~doc:"Hostname to resolve" ["hostname"] in
  Key.(create "hostname" Arg.(opt string "robur.io" doc))

let dns_handler =
  let packages =
    [
      package "logs" ;
      package ~min:"4.5.0" ~sublibs:[ "mirage" ] "dns-client";
    ]
  in
  foreign
    ~keys:[Key.abstract hostname]
    ~packages
    "Unikernel.Main" (random @-> time @-> mclock @-> stackv4 @-> job)

let () =
  register "client" [dns_handler $ default_random $ default_time $ default_monotonic_clock $ generic_stackv4 default_network ]
