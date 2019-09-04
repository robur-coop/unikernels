(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let hostname =
  let doc = Key.Arg.info ~doc:"Hostname to resolve" ["hostname"] in
  Key.(create "hostname" Arg.(opt string "robur.io" doc))

let dns_handler =
  let packages =
    [
      package "logs" ;
      package ~sublibs:[ "mirage" ] "dns-client";
    ]
  in
  foreign
    ~keys:[Key.abstract hostname]
    ~deps:[abstract nocrypto]
    ~packages
    "Unikernel.Main" (random @-> stackv4 @-> job)

let () =
  register "client" [dns_handler $ default_random $ generic_stackv4 default_network ]
