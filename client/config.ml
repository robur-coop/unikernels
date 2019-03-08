(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let dns_handler =
  let packages =
    let pin = "git+https://github.com/roburio/udns.git" in
    [
      package "logs" ;
      package ~pin "udns";
      package ~pin "udns-client";
      package ~pin "udns-mirage-client";
    ]
  in
  foreign
    ~deps:[abstract nocrypto]
    ~packages
    "Unikernel.Main" (stackv4 @-> job)

let () =
  register "client" [dns_handler $ generic_stackv4 default_network ]
