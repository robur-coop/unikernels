(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let port =
  let doc = Key.Arg.info ~doc:"The TCP port on which to listen for incoming connections." ["port"] in
  Key.(create "port" Arg.(opt int 443 doc))

let dns_key =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" ["dns-key"] in
  Key.(create "dns-key" Arg.(required string doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"dns server IP" ["dns-server"] in
  Key.(create "dns-server" Arg.(required ipv4_address doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"dns server port" ["dns-port"] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let hostname =
  let doc = Key.Arg.info ~doc:"hostname" ["hostname"] in
  Key.(create "hostname" Arg.(required string doc))

let additional =
  let doc = Key.Arg.info ~doc:"additional" ["additional"] in
  Key.(create "additional" Arg.(opt string "" doc))

let key_seed =
  let doc = Key.Arg.info ~doc:"private key seed" ["key-seed"] in
  Key.(create "key-seed" Arg.(opt (some string) None doc))

let keys = Key.[
    abstract port ; abstract dns_key ; abstract dns_server ; abstract dns_port ;
    abstract hostname ; abstract additional ; abstract key_seed
  ]

let packages =
  [
    package "x509" ;
    package "duration" ;
    package "randomconv" ;
    package "logs" ;
    package ~sublibs:[ "mirage" ] "dns-certify";
    package "tls-mirage" ;
  ]

let main =
  foreign ~keys ~packages "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4 @-> job)

let () =
  register "certificate" [
    main $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4 default_network
  ]
