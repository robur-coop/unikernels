(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) = struct

  module D = Dns_mirage_server.Make(P)(M)(T)(S)

  let data =
    let open Dns in
    let n = Domain_name.of_string_exn
    and ip = Ipaddr.V4.of_string_exn
    and s = Domain_name.Set.singleton
    in
    let s_ip ipaddr = Rr_map.Ipv4_set.singleton (ip ipaddr) in
    let domain = n "mirage" in
    let m = Domain_name.prepend_exn domain in
    let ns = m "ns"
    and ttl = 2560l
    in
    let soa = { Dns.Soa.nameserver = ns ;
                hostmaster = m "hostmaster" ;
                serial = 1l ; refresh = 10l ; retry = 5l ;
                expiry = 600l ; minimum = ttl }
    in
    let open Dns_trie in
    let open Rr_map in
    let t = insert domain Soa soa Dns_trie.empty in
    let t = insert domain Ns (ttl, s ns) t in
    let t = insert (m "router") A (ttl, s_ip "10.0.42.1") t in
    let t = insert ns A (ttl, s_ip "10.0.42.2") t in
    let t = insert (m "charrua") A (ttl, s_ip "10.0.42.3") t in
    let t = insert (m "secondary") A (ttl, s_ip "10.0.42.4") t in
    let t = insert (m "resolver") A (ttl, s_ip "10.0.42.5") t in
    let t = insert (m "letsencrypt") A (ttl, s_ip "10.0.42.6") t in
    let t = insert (m "certificate") A (ttl, s_ip "10.0.42.7") t in
    let t = insert (m "www") Cname (ttl, m "router") t in
    let ptr_zone = n "42.0.10.in-addr.arpa" in
    let ptr_soa = { Dns.Soa.nameserver = ns ;
                    hostmaster = n "hostmaster.example" ;
                    serial = 1l ; refresh = 16384l ; retry = 2048l ;
                    expiry = 1048576l ; minimum = ttl }
    in
    let ptr_name = Domain_name.prepend_exn ptr_zone in
    let t = insert ptr_zone Soa ptr_soa t in
    let t = insert ptr_zone Ns (ttl, s ns) t in
    let t = insert (ptr_name "1") Ptr (ttl, m "router") t in
    let t = insert (ptr_name "2") Ptr (ttl, m "ns") t in
    let t = insert (ptr_name "3") Ptr (ttl, m "charrua") t in
    let t = insert (ptr_name "4") Ptr (ttl, m "secondary") t in
    let t = insert (ptr_name "5") Ptr (ttl, m "resolver") t in
    let t = insert (ptr_name "6") Ptr (ttl, m "letsencrypt") t in
    let t = insert (ptr_name "7") Ptr (ttl, m "certificate") t in
    t

  let start _rng pclock mclock _ s _ =
    let trie = data in
    (match Dns_trie.check trie with
     | Ok () -> ()
     | Error e ->
       Logs.err (fun m -> m "error %a during check()" Dns_trie.pp_zone_check e) ;
       invalid_arg "check") ;
    let keys =
      let key key =
        let key = Cstruct.of_string key in
        { Dns.Dnskey.flags = 0 ; algorithm = SHA256 ; key }
      in
      [
        Domain_name.of_string_exn ~hostname:false "10.0.42.2.10.0.42.4._transfer.mirage" ,
        key "G/7zDZr98BTzoi9N6HEUFOg7byKfH9rsPav5JMm9l8Y=" ;
        Domain_name.of_string_exn ~hostname:false "barf.10.0.42.2._transfer.mirage" ,
        key "sCgZ0SgEaFbpBxv+n74bognpLdR7gdutn8lO0/wpGJY=" ;
        Domain_name.of_string_exn ~hostname:false "key._transfer.mirage" ,
        key "/WcnjpqrErYrXi1dd4sv8dfwCwDFg0ZGm6N6Bq1VwMI=" ;
        Domain_name.of_string_exn ~hostname:false "one._update.mirage" ,
        key "eRhj4OoaGIIJ3I9hJFwYGhAkdiR5DNzia0WoGrYy70k=" ;
      ]
    in
    let t =
      Dns_server.Primary.create ~keys
        ~a:[Dns_server.Authentication.tsig_auth] ~tsig_verify:Dns_tsig.verify
        ~tsig_sign:Dns_tsig.sign ~rng:R.generate trie
    in
    Logs.info (fun m -> m "loaded zone: %a"
                  (Rresult.R.pp ~ok:Fmt.string ~error:Rresult.R.pp_msg)
                  (Dns_server.text (Domain_name.of_string_exn "mirage") trie)) ;
    D.primary s t ;
    S.listen s
end
