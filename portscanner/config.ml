open Mirage

let main =
  let packages = [package ~sublibs:["ipv4";"udp";"tcp"] "tcpip"; package "ethernet"; package "arp"; package "arp-mirage"] in
  foreign
    ~packages
    "Unikernel.Main" (console @-> network @-> mclock @-> time @-> random @-> job)

let () =
  register "ethifv4" [
    main $ default_console $ default_network $ default_monotonic_clock $ default_time $ default_random
  ]
