(* (c) 2017-2019 Hannes Mehnert, all rights reserved *)

open Mirage

let user =
  let doc = Key.Arg.info ~doc:"user" ["user"] in
  Key.(create "user" Arg.(opt string "hannes" doc))

let key_seed =
  let doc = Key.Arg.info ~doc:"key seed" ["seed"] in
  Key.(create "seed" Arg.(opt string "180586" doc))

let authenticator =
  let doc = Key.Arg.info ~doc:"authenticator" ["authenticator"] in
  Key.(create "authenticator" Arg.(opt string "" doc))

let host =
  let doc = Key.Arg.info ~doc:"Host to connect to" ["host"] in
  Key.(create "host" Arg.(opt string "10.0.42.1" doc))

let command =
  let doc = Key.Arg.info ~doc:"command to execute" ["command"] in
  Key.(create "command" Arg.(opt string "ls" doc))

let dns_handler =
  let packages =
    [
      package "awa-mirage";
    ]
  in
  foreign
    ~keys:[Key.abstract user ; Key.abstract key_seed ; Key.abstract authenticator ; Key.abstract host ; Key.abstract command ]
    ~deps:[abstract nocrypto]
    ~packages
    "Unikernel.Main" (mclock @-> stackv4 @-> job)

let () =
  register "client" [dns_handler $ default_monotonic_clock $ generic_stackv4 default_network ]
