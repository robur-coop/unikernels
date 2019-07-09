(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let dns_handler =
  let packages =
    let pin = "git+https://github.com/mirage/ocaml-dns.git" in
    [
      package "logs" ;
      package "ethernet";
      package "arp";
      package "arp-mirage";
      package "ipaddr";
      package "tcpip" ~sublibs:["stack-direct"; "icmpv4"; "ipv4"; "udp"; "tcp"];
      package "mirage-qubes";
      package "mirage-qubes-ipv4";
      package ~pin "dns";
      package ~pin "dns-mirage";
      package ~pin "dns-resolver";
      package "randomconv" ;
      package "lru" ;
      package "rresult" ;
      package "duration" ;
    ]
  in
  foreign
    ~deps:[abstract nocrypto]
    ~packages
    "Unikernel.Main" (random @-> pclock @-> mclock @-> time @-> network @-> qubesdb @-> job)

let db = default_qubesdb

let () =
  register "resolver" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ default_network $ db ]
