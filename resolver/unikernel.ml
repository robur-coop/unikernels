(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Mirage_types_lwt

let data =
  let n = Domain_name.of_string_exn
  and ip = Ipaddr.V4.of_string_exn
  and s = Domain_name.Set.singleton
  in
  let domain = n "mirage" in
  let m = Domain_name.prepend_exn domain in
  let ns = m "ns"
  and ttl = 2560l
  in
  let ip_set i = Dns_map.Ipv4Set.singleton i in
  let ptr_zone = n "42.168.192.in-addr.arpa" in
  let ptr_name = Domain_name.prepend_exn ptr_zone in
  let records = [
    (m "router", ip "192.168.42.1", ptr_name "1") ;
    (m "dhcp", ip "192.168.42.2", ptr_name "2") ;
    (ns, ip "192.168.42.3", ptr_name "3") ;
    (m "opam", ip "192.168.42.5", ptr_name "5") ;
    (m "pad", ip "192.168.42.6", ptr_name "6") ;
    (m "monitor", ip "192.168.42.7", ptr_name "7") ;
    (domain, ip "192.168.42.8", ptr_name "8") ;
  ] in
  let soa = Dns_packet.({ nameserver = ns ;
                          hostmaster = m "hostmaster" ;
                          serial = 1l ; refresh = 10l ; retry = 5l ;
                          expiry = 600l ; minimum = ttl })
  in
  let open Dns_map in
  let open Dns_trie in
  let t = insert domain Soa (ttl, soa) empty in
  let t = insert domain Ns (ttl, s ns) t in
  let t =
    List.fold_left
      (fun trie (name, ip, _) -> insert name A (ttl, ip_set ip) trie)
      t records
  in
  let ptr_soa = Dns_packet.({ nameserver = ns ;
                              hostmaster = n "hostmaster.example" ;
                              serial = 42l ; refresh = 16384l ; retry = 2048l ;
                              expiry = 1048576l ; minimum = ttl })
  in
  let t = insert ptr_zone Soa (ttl, ptr_soa) t in
  let t = insert ptr_zone Ns (ttl, s ns) t in
  let t = insert (ptr_name "4") Ptr (ttl, n "cal.robur.io") t in
  List.fold_left
    (fun trie (name, _, ptr) -> insert ptr Ptr (ttl, name) trie)
    t records

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) = struct
  module D = Dns_mirage_resolver.Make(R)(P)(M)(T)(S)

  let start _r pclock mclock _ s _ =
    let trie =
      List.fold_left
        (fun trie (k, v) -> Dns_trie.insertb k v trie)
        data Dns_resolver_root.reserved_zones
    in
    let keys = [
      Domain_name.of_string_exn ~hostname:false "foo._key-management" ,
      { Dns_packet.flags = 0 ; key_algorithm = Dns_enum.SHA256 ; key = Cstruct.of_string "/NzgCgIc4yKa7nZvWmODrHMbU+xpMeGiDLkZJGD/Evo=" } ;
      (Domain_name.of_string_exn ~hostname:false "dhcp._update",
       { Dns_packet.flags = 0 ; key_algorithm = Dns_enum.SHA256 ; key = Cstruct.of_string "1W1QTpMwpNqnikpYztxjaKmltDpsmvWW70+itztu82g=" })
    ] in
    (match Dns_trie.check trie with
     | Ok () -> ()
     | Error e ->
       Logs.err (fun m -> m "check after update returned %a" Dns_trie.pp_err e)) ;
    let now = M.elapsed_ns mclock in
    let server =
      UDns_server.Primary.create ~keys ~a:[UDns_server.Authentication.tsig_auth]
        ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign ~rng:R.generate
        trie
    in
    let p = UDns_resolver.create now R.generate server in
    D.resolver ~timer:1000 ~root:true s p ;
    S.listen s
end
