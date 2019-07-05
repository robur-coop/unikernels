(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let keys =
  let doc = Key.Arg.info ~doc:"nsupdate keys (name:type:value,...)" ["keys"] in
  Key.(create "keys" Arg.(opt (list string) [] doc))

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote git repository." ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/roburio/udns.git" doc))

let dns_handler =
  let udns_pin = "git+https://github.com/roburio/udns.git" in
  let irmin_pin = "git+https://github.com/hannesm/irmin.git#minor" in
  let git_pin = "git+https://github.com/hannesm/ocaml-git.git#minor" in
  let packages = [
    package "logs" ;
    package ~pin:udns_pin "dns";
    package ~pin:udns_pin "dns-client";
    package ~pin:udns_pin "dns-mirage-client";
    package ~pin:udns_pin "dns-mirage";
    package ~pin:udns_pin "dns-server";
    package ~pin:udns_pin "dns-zone";
    package ~pin:udns_pin "dns-mirage-server";
    package ~pin:udns_pin "dns-tsig";
    package "nocrypto" ;
    package ~pin:irmin_pin "irmin";
    package ~pin:irmin_pin "irmin-git";
    package ~pin:irmin_pin "irmin-graphql";
    package ~pin:irmin_pin "irmin-mem";
    package ~pin:irmin_pin "irmin-mirage";
    package ~pin:"git+https://github.com/hannesm/encore.git#minor" "encore";
    package ~pin:git_pin "git";
    package ~pin:git_pin "git-http";
    package ~pin:git_pin "git-mirage";
    package ~pin:"git+https://github.com/hannesm/ocaml-conduit.git#udns" "mirage-conduit";
    package ~pin:"git+https://github.com/mirage/ke.git" "ke"
  ] in
  foreign
    ~deps:[abstract nocrypto]
    ~keys:[Key.abstract remote_k ; Key.abstract keys]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4 @-> resolver @-> conduit @-> job)

let () =
  let net = generic_stackv4 default_network in
  register "primary-git"
    [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $
     default_time $ net $ resolver_dns net $ conduit_direct ~tls:true net]
