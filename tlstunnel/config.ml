(* (c) 2019 Hannes Mehnert, all rights reserved *)

open Mirage

let backend_ip =
  let doc = Key.Arg.info ~doc:"Backend IP address" ["backend-ip"] in
  Key.(create "backend_ip" Arg.(opt ipv4_address Ipaddr.V4.localhost doc))

let backend_port =
  let doc = Key.Arg.info ~doc:"The TCP port of the backend." ["backend-port"] in
  Key.(create "backend_port" Arg.(opt int 80 doc))

let frontend_port =
  let doc = Key.Arg.info ~doc:"The TCP port of the frontend." ["frontend-port"] in
  Key.(create "frontend_port" Arg.(opt int 443 doc))

let certs = generic_kv_ro "tls"

let main =
  foreign
    ~keys:[ Key.abstract backend_ip ; Key.abstract backend_port ; Key.abstract frontend_port ]
    ~packages:[ package "tls-mirage" ]
    "Unikernel.Main"
    (kv_ro @-> pclock @-> stackv4 @-> stackv4 @-> job)

let stack = generic_stackv4 default_network

let stack2 = generic_stackv4 ~group:"private" (netif ~group:"private" "private")

let () =
  register "tlstunnel"
    [ main $ certs $ default_posix_clock $ stack $ stack2 ]
