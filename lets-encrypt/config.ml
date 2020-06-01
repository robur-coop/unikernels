(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Mirage

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

let email =
  let doc = Key.Arg.info ~doc:"Contact eMail address for let's encrypt" ["email"] in
  Key.(create "email" Arg.(opt (some string) None doc))

let monitor =
  let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
  Key.(create "monitor" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let syslog =
  let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
  Key.(create "syslog" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let name =
  let doc = Key.Arg.info ~doc:"Name of the unikernel" ["name"] in
  Key.(create "name" Arg.(opt string "sn.nqsb.io" doc))

let keys = [
  Key.abstract dns_key ; Key.abstract dns_server ; Key.abstract port ;
  Key.abstract account_key_seed ; Key.abstract production ; Key.abstract email ;
  Key.abstract name ; Key.abstract syslog ; Key.abstract monitor ;
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
    package ~min:"4.5.99" "dns-certify";
    package ~min:"4.4.0" ~sublibs:[ "mirage" ] "dns-server";
    package "randomconv" ;
    package ~min:"0.3.0" "domain-name" ;
    package ~sublibs:["mirage"] "logs-syslog";
    package "monitoring-experiments";
]

let client =
  foreign ~keys ~packages "Unikernel.Client" @@
  console @-> random @-> pclock @-> mclock @-> time @-> stackv4 @-> resolver @-> conduit @-> job

let management_stack = generic_stackv4 ~group:"management" (netif ~group:"management" "management")

let () =
  let res_dns = resolver_dns management_stack in
  let conduit = conduit_direct management_stack in
  register "letsencrypt"
    [ client $ default_console $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ management_stack $ res_dns $ conduit ]
