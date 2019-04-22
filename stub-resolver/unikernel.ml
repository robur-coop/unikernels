(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) = struct
  module D = Udns_mirage_resolver.Make(R)(P)(M)(T)(S)

  let start _r pclock mclock _ s _ =
    let trie =
      let name = Domain_name.of_string_exn "resolver"
      and ip = Key_gen.resolver ()
      in
      let trie =
        Udns_trie.insert Domain_name.root
          Udns.Rr_map.Ns (300l, Domain_name.Set.singleton name)
          Udns_resolver_root.reserved
      in
      Udns_trie.insert name Udns.Rr_map.A
        (300l, Udns.Rr_map.Ipv4_set.singleton ip)
        trie
    in
    (match Udns_trie.check trie with
     | Ok () -> ()
     | Error e ->
       Logs.err (fun m -> m "check after update returned %a" Udns_trie.pp_zone_check e)) ;
    let now = M.elapsed_ns mclock in
    let server =
      Udns_server.Primary.create ~a:[Udns_server.Authentication.tsig_auth]
        ~tsig_verify:Udns_tsig.verify ~tsig_sign:Udns_tsig.sign ~rng:R.generate
        trie
    in
    let p = Udns_resolver.create ~mode:`Stub now R.generate server in
    D.resolver s p ;
    S.listen s
end
