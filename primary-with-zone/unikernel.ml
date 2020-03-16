(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
open Lwt.Infix

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Mirage_stack.V4) (KV : Mirage_kv.RO) = struct

  module D = Dns_server_mirage.Make(P)(M)(T)(S)

  let start _rng _pclock _mclock _ s kv =
    KV.get kv (Mirage_kv.Key.v "zone") >>= function
    | Error e ->
      Logs.err (fun m -> m "couldn't get zone file %a" KV.pp_error e) ; exit 64
    | Ok data -> match Dns_zone.parse data with
      | Error (`Msg msg) ->
        Logs.err (fun m -> m "zonefile.load: %s" msg) ; exit 64
      | Ok rrs ->
        let trie = Dns_trie.insert_map rrs Dns_trie.empty in
        match Dns_trie.check trie with
         | Error e ->
           Logs.err (fun m -> m "error %a during check()" Dns_trie.pp_zone_check e) ; exit 64
         | Ok () ->
           let t = Dns_server.Primary.create ~rng:R.generate trie in
           D.primary s t ;
           S.listen s
end
