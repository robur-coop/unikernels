(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
open Lwt.Infix

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Mirage_stack.V4) = struct
  module D = Dns_server_mirage.Make(P)(M)(T)(S)
  module Store = Irmin_unix.Git.FS.KV(* (Irmin_unix.Git.G) *)(Irmin.Contents.String)

  let info = Irmin_unix.info ~author:"udns-git"

  let start _rng _pclock _mclock _ s _ =
    let keys = List.fold_left (fun acc key ->
        match Dns.Dnskey.name_key_of_string key with
        | Error (`Msg msg) -> Logs.err (fun m -> m "key parse error: %s" msg) ; exit 64
        | Ok (name, key) -> (name, key) :: acc)
        [] (Key_gen.keys ())
    in
    let root = Key_gen.repo () in
    let config = Irmin_git.config ~bare:true root in
    Store.Repo.v config >>= fun repo ->
    let t =
      Dns_server.Secondary.create ~rng:R.generate
        ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign
        keys
    in
    let on_update ~old:_ t =
      (* find zones, text all of them *)
      Store.master repo >>= fun branch ->
      let trie = Dns_server.Secondary.data t in
      let zones =
        Dns_trie.fold Dns.Rr_map.Soa trie (fun zone _ acc ->
            match Dns_server.text zone trie with
            | Error (`Msg str) ->
              Logs.err (fun m -> m "updated zone %a, but failed text %s" Domain_name.pp zone str) ;
              acc
            | Ok str ->
              (* store it in git *)
              Logs.info (fun m -> m "updated zone %a\n%s" Domain_name.pp zone str) ;
              (zone, str) :: acc) []
      in
      Lwt_list.iter_s (fun (zone, data) ->
          let k = [ Domain_name.to_string zone ] in
          Store.find branch k >>= (function
              | Some old_str when String.equal data old_str ->
                Logs.info (fun m -> m "nothing to do here") ;
                Lwt.return_unit
              | _ ->
                Store.set branch ~info:(info "zone transferred") k data >|= function
                | Error _ -> Logs.warn (fun m -> m "Store.set failed")
                | Ok () -> ()) >|= fun () ->
          (* try to load it again... just in case ;) *)
          match Dns_zone.parse data with
            | Error (`Msg msg) ->
              Logs.err (fun m -> m "error while loading zonefile: %s" msg)
            | Ok rrs ->
              (* now insert it into a Dns_trie *)
              let trie = Dns_trie.insert_map rrs Dns_trie.empty in
              (* check that *)
              (match Dns_trie.check trie with
               | Ok () -> ()
               | Error e ->
                 Logs.err (fun m -> m "error %a during check()" Dns_trie.pp_zone_check e)) ;
              (* and generate a zonefile from the trie *)
              match Dns_server.text zone trie with
              | Error (`Msg str) ->
                Logs.err (fun m -> m "failed to produce zone %a second time %s"
                             Domain_name.pp zone str)
              | Ok str' ->
                (* and complain loudly if not equal *)
                let equal = String.equal data str' in
                Logs.info (fun m -> m "generated zone (equal %b) %a:%s"
                              equal Domain_name.pp zone (if equal then "" else str')))
        zones
    in
    let port = Key_gen.port () in
    D.secondary ~on_update ~port s t ;
    S.listen s
end
