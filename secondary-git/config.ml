(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let address =
  let network = Ipaddr.V4.Prefix.of_address_string_exn "10.0.42.4/24"
  and gateway = Some (Ipaddr.V4.of_string_exn "10.0.42.1")
  in
  { network ; gateway }

let keys =
  let doc = Key.Arg.info ~doc:"nsupdate keys (name:type:value,...)" ["keys"] in
  Key.(create "keys" Arg.(opt (list string) [] doc))

let repo =
  let doc = Key.Arg.info ~doc:"git repository" ["repo"] in
  Key.(create "repo" Arg.(required string doc))

let port =
  let doc = Key.Arg.info ~doc:"listen port" ["port"] in
  Key.(create "port" Arg.(opt int 53 doc))

let dns_handler =
  let packages =
    [
      package "logs" ;
      package ~min:"4.3.0" ~sublibs:["mirage" ; "zone"] "dns-server";
      package "dns-tsig";
      package "nocrypto" ;
      package ~min:"2.0.0" "irmin-unix" ;
    ]
  and keys = Key.([ abstract keys ; abstract repo ; abstract port ])
  in
  foreign
    ~deps:[abstract nocrypto]
    ~keys
    ~packages
    "Unikernel.Main" (random @-> pclock @-> mclock @-> time @-> stackv4 @-> job)

let () =
  register "secondary-git" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4 default_network ]
