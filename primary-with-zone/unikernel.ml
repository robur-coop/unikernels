(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) (KV : KV_RO) = struct

  module D = Udns_mirage_server.Make(P)(M)(T)(S)

  let start _rng pclock mclock _ s kv _ =
    KV.get kv (Mirage_kv.Key.v "zone") >>= function
    | Error e -> Lwt.fail_with "couldn't get zone file"
    | Ok data -> match Udns_zone.parse data with
      | Error (`Msg msg) ->
        Logs.err (fun m -> m "zonefile.load: %s" msg) ;
        Lwt.fail_with "zone parser"
      | Ok rrs ->
        let trie = Udns_trie.insert_map rrs Udns_trie.empty in
        match Udns_trie.check trie with
         | Error e ->
           Logs.err (fun m -> m "error %a during check()" Udns_trie.pp_zone_check e) ;
           Lwt.fail_with "check failed"
         | Ok () ->
           let t =
             Udns_server.Primary.create ~a:[Udns_server.Authentication.tsig_auth]
               ~tsig_verify:Udns_tsig.verify ~tsig_sign:Udns_tsig.sign
               ~rng:R.generate trie
           in
           D.primary s t ;
           S.listen s
end
