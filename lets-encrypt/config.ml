(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Mirage

(* what should be the boot parameters?
   - DNS update key, zone, and IP address
   - hostname and certificate key seed
   - account key seed
*)

let dns_keys =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" ["dns-keys"] in
  Key.(create "dns-keys" Arg.(opt (list string) [] doc))

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
    abstract dns_keys ; abstract dns_server ; abstract port ;
    abstract account_key_seed ; abstract production
  ]

let packages =
  let pin = "git+https://github.com/roburio/udns.git" in
  [
    package "x509" ;
    package "duration" ;
    package "logs" ;
    package "cohttp-mirage" ;
    package ~pin:"git+https://github.com/hannesm/ocaml-letsencrypt.git#udns" "letsencrypt" ;
    package ~pin:"git+https://github.com/hannesm/ocaml-conduit.git#udns" "mirage-conduit" ;
    package ~pin "dns";
    package ~pin "dns-client";
    package ~pin "dns-mirage-client";
    package ~pin "dns-tsig";
    package ~pin "dns-mirage";
    package ~pin "dns-mirage-server";
    package ~pin "dns-server";
    package "randomconv" ;
]

let client =
  foreign ~deps:[abstract nocrypto] ~keys ~packages "Unikernel.Client" @@
  random @-> pclock @-> mclock @-> time @-> stackv4 @-> resolver @-> conduit @-> job

let () =
  let net = generic_stackv4 default_network in
  let res_dns = resolver_dns net in
  let conduit = conduit_direct net in
  register "letsencrypt"
    [ client $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ net $ res_dns $ conduit ]
