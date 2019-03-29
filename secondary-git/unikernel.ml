(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) = struct
  module D = Udns_mirage_server.Make(P)(M)(T)(S)
  module Store = Irmin_unix.Git.FS.KV(* (Irmin_unix.Git.G) *)(Irmin.Contents.String)

  let info = Irmin_unix.info ~author:"udns-git"

  let start _rng pclock mclock _ s _ =
    let keys = List.fold_left (fun acc key ->
        match Udns_packet.name_dnskey_of_string str with
        | Error (`Msg msg) -> Logs.err (fun m -> m "key parse error: %s" msg) ; acc
        | Ok (name, key) -> (name, key) :: acc)
        [] (Key_gen.keys ())
    in
    let root = Key_gen.repo () in
    let config = Irmin_git.config ~bare:true root in
    Store.Repo.v config >>= fun repo ->
    let t =
      Udns_server.Secondary.create ~a:[ Udns_server.Authentication.tsig_auth ]
        ~tsig_verify:Udns_tsig.verify ~tsig_sign:Udns_tsig.sign
        ~rng:R.generate keys
    in
    let on_update t =
      (* find zones, text all of them *)
      let zones = Udns_server.Secondary.zones t in
      let server = Udns_server.Secondary.server t in
      Store.master repo >>= fun branch ->
      Lwt_list.iter_s (fun zone ->
          match Udns_server.text zone server with
          | Error str ->
            Logs.err (fun m -> m "updated zone %a, but failed text %s" Domain_name.pp zone str) ;
            Lwt.return_unit
          | Ok str ->
            (* store it in git *)
            Logs.info (fun m -> m "updated zone %a\n%s" Domain_name.pp zone str) ;
            let k = [ Domain_name.to_string zone ] in
            Store.find branch k >>= (function
                | Some old_str when String.equal str old_str ->
                  Logs.info (fun m -> m "nothing to do here") ;
                  Lwt.return_unit
                | _ ->
                  Store.set branch ~info:(info "zone transferred") k str >|= function
                  | Error e -> Logs.warn (fun m -> m "Store.set failed")
                  | Ok () -> ()) >|= fun () ->
            (* try to load it again... just in case ;) *)
            match Udns_zonefile.load str with
            | Error msg ->
              Logs.err (fun m -> m "error while loading zonefile: %s" msg)
            | Ok rrs ->
              (* now insert it into a Udns_trie *)
              let trie = Udns_trie.insert_map (Udns_map.of_rrs rrs) Udns_trie.empty in
              (* check that *)
              (match Udns_trie.check trie with
               | Ok () -> ()
               | Error e ->
                 Logs.err (fun m -> m "error %a during check()" Udns_trie.pp_err e)) ;
              (* and generate a zonefile from the trie *)
              let s = Udns_server.Secondary.with_data t trie in
              match Udns_server.text zone trie with
              | Error str ->
                Logs.err (fun m -> m "failed to produce zone %a second time %s"
                             Domain_name.pp zone str)
              | Ok str' ->
                (* and complain loudly if not equal *)
                let equal = String.equal str str' in
                Logs.info (fun m -> m "generated zone (equal %b) %a:%s"
                              equal Domain_name.pp zone (if equal then "" else str')))
        zones
    in
    let port = Key_gen.port () in
    D.secondary ~on_update ~port s t ;
    S.listen s
end
