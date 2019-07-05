(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let keys =
  let doc = Key.Arg.info ~doc:"nsupdate keys (name:type:value,...)" ["keys"] in
  Key.(create "keys" Arg.(opt (list string) [] doc))

let dns_handler =
  let packages =
    let pin = "git+https://github.com/roburio/udns.git" in
    [
      package "logs" ;
      package ~pin "dns";
      package ~pin "dns-mirage";
      package ~pin "dns-server";
      package ~pin "dns-mirage-server";
      package ~pin "dns-tsig";
      package "nocrypto";
    ]
  and keys = Key.([ abstract keys ])
  in
  foreign
    ~deps:[abstract nocrypto]
    ~keys
    ~packages
    "Unikernel.Main" (random @-> pclock @-> mclock @-> time @-> stackv4 @-> job)

let () =
  register "secondary" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4 default_network ]
