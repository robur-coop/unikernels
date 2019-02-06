(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote repository to fetch content."
      ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https//github.com/roburio/udns.git" doc))

let dns_handler =
  let packages = [
    package "logs" ;
    package ~sublibs:[ "server" ; "zonefile" ; "mirage.server" ; "crypto" ]
      ~pin:"git+https://github.com/roburio/udns.git" "udns" ;
    package "nocrypto" ;
    package ~min:"1.0.0" "irmin";
    package "irmin-mirage";
  ] in
  foreign
    ~deps:[abstract nocrypto]
    ~keys:[Key.abstract remote_k]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4 @-> resolver @-> conduit @-> job)

let () =
  let net = generic_stackv4 default_network in
  register "primary-git"
    [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $
     default_time $ net $ resolver_dns net $ conduit_direct net]
