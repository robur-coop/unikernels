(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Mirage

(* what should be the boot parameters?
   - DNS update key, zone, and IP address
   - hostname and certificate key seed
   - account key seed
*)

let dns_key =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" ["dns-key"] in
  Key.(create "dns-key" Arg.(opt string "" doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"dns server IP" ["dns-server"] in
  Key.(create "dns-server" Arg.(opt ipv4_address Ipaddr.V4.localhost doc))

let port =
  let doc = Key.Arg.info ~doc:"dns server port" ["port"] in
  Key.(create "port" Arg.(opt int 53 doc))

let account_key_seed =
  let doc = Key.Arg.info ~doc:"account key seed" ["account-key-seed"] in
  Key.(create "account-key-seed" Arg.(opt string "" doc))

let production =
  let doc = Key.Arg.info ~doc:"Use the production let's encrypt servers" ["production"] in
  Key.(create "production" Arg.(flag doc))

let keys = Key.[
    abstract dns_key ; abstract dns_server ; abstract port ;
    abstract account_key_seed ; abstract production
  ]

let packages =
  [
    package ~min:"0.9.0" "x509" ;
    package "duration" ;
    package "logs" ;
    package "cohttp-mirage" ;
    package ~min:"0.2.1" "letsencrypt" ;
    package "conduit-mirage" ;
    package "dns-tsig";
    package "dns-certify";
    package ~min:"4.3.0" ~sublibs:[ "mirage" ] "dns-server";
    package "randomconv" ;
    package ~min:"0.3.0" "domain-name"
]

let client =
  foreign ~keys ~packages "Unikernel.Client" @@
  random @-> pclock @-> mclock @-> time @-> stackv4 @-> resolver @-> conduit @-> job

let () =
  let net = generic_stackv4 default_network in
  let res_dns = resolver_dns net in
  let conduit = conduit_direct net in
  register "letsencrypt"
    [ client $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ net $ res_dns $ conduit ]
