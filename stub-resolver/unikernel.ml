(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Mirage_stack.V4) = struct
  module D = Dns_resolver_mirage.Make(R)(P)(M)(T)(S)

  let start _r _pclock _mclock _ s _ =
    let trie =
      let name = Domain_name.of_string_exn "resolver"
      and ip = Key_gen.resolver ()
      in
      let trie =
        Dns_trie.insert Domain_name.root
          Dns.Rr_map.Ns (300l, Domain_name.(Host_set.singleton (host_exn name)))
          Dns_resolver_root.reserved
      in
      Dns_trie.insert name Dns.Rr_map.A
        (300l, Dns.Rr_map.Ipv4_set.singleton ip)
        trie
    in
    (match Dns_trie.check trie with
     | Ok () -> ()
     | Error e ->
       Logs.err (fun m -> m "check after update returned %a" Dns_trie.pp_zone_check e)) ;
    let now = M.elapsed_ns () in
    let server = Dns_server.Primary.create ~rng:R.generate trie in
    let p = Dns_resolver.create ~mode:`Stub now R.generate server in
    D.resolver s p ;
    S.listen s
end
