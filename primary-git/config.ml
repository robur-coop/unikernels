(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote git repository." ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/roburio/udns.git" doc))

let axfr =
  let doc = Key.Arg.info ~doc:"Allow unauthenticated zone transfer." ["axfr"] in
  Key.(create "axfr" Arg.(flag doc))

let seed =
  let doc = Key.Arg.info ~doc:"Seed for private key." ["seed"] in
  Key.(create "seed" Arg.(opt string "" doc))

let authenticator =
  let doc = Key.Arg.info ~doc:"Authenticator." ["authenticator"] in
  Key.(create "authenticator" Arg.(opt string "" doc))

let monitor =
  let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
  Key.(create "monitor" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let syslog =
  let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
  Key.(create "syslog" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let name =
  let doc = Key.Arg.info ~doc:"Name of the unikernel" ["name"] in
  Key.(create "name" Arg.(opt string "ns.nqsb.io" doc))

let dns_handler =
  let packages = [
    package "logs" ;
    package ~min:"4.3.0" ~sublibs:["mirage"; "zone"] "dns-server";
    package "dns-tsig";
    package ~min:"2.0.0" "irmin-mirage";
    package ~min:"2.0.0" "irmin-mirage-git";
    package ~min:"2.2.98" "conduit-mirage";
    package "monitoring-experiments";
    package ~sublibs:["mirage"] "logs-syslog";
  ] in
  foreign
    ~keys:[
      Key.abstract remote_k ; Key.abstract axfr ; Key.abstract seed ; Key.abstract authenticator ;
      Key.abstract name ; Key.abstract monitor ; Key.abstract syslog
    ]
    ~packages
    "Unikernel.Main"
    (console @-> random @-> pclock @-> mclock @-> time @-> stackv4 @-> resolver @-> conduit @-> stackv4 @-> job)

let management_stack = generic_stackv4 ~group:"management" (netif ~group:"management" "management")

let () =
  let net = generic_stackv4 default_network in
  register "primary-git"
    [dns_handler $ default_console $ default_random $ default_posix_clock $ default_monotonic_clock $
     default_time $ net $ resolver_dns net $ conduit_direct ~tls:true net $ management_stack]
