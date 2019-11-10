(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let keys =
  let doc = Key.Arg.info ~doc:"nsupdate keys (name:type:value,...)" ["keys"] in
  Key.(create "keys" Arg.(opt (list string) [] doc))

let monitor =
  let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
  Key.(create "monitor" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let syslog =
  let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
  Key.(create "syslog" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let name =
  let doc = Key.Arg.info ~doc:"Name of the unikernel" ["name"] in
  Key.(create "name" Arg.(opt string "sn.nqsb.io" doc))

let dns_handler =
  let packages =
    [
      package "logs" ;
      package ~min:"4.3.0" ~sublibs:["mirage"] "dns-server";
      package "dns-tsig";
      package "monitoring-experiments";
      package ~sublibs:["mirage"] "logs-syslog";
    ]
  and keys = [
    Key.abstract keys ;
    Key.abstract name ; Key.abstract monitor ; Key.abstract syslog
  ]
  in
  foreign
    ~deps:[abstract app_info]
    ~keys
    ~packages
    "Unikernel.Main" (console @-> random @-> pclock @-> mclock @-> time @-> stackv4 @-> stackv4 @-> job)

let management_stack = generic_stackv4 ~group:"management" (netif ~group:"management" "management")
let service_stack = generic_stackv4 (netif ~group:"as250" "as250")

let () =
  register "secondary" [dns_handler $ default_console $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ service_stack $ management_stack ]
