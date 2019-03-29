(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) = struct

  module ROStore = Irmin_mirage.Git.KV_RO(Irmin_git.Mem)

  let load_zones conduit resolver =
    (* TODO recurse over directories (io/nqsb, org/openmirage) for zones *)
    (* TODO check that each file only contains records that are subdomains of filename *)
    Irmin_git.Mem.v (Fpath.v ".") >>= function
    | Error _ -> assert false
    | Ok git ->
      ROStore.connect git ~conduit ~resolver (Key_gen.remote ()) >>= fun store ->
      ROStore.list store Mirage_kv.Key.empty >>= function
      | Error e ->
        Logs.err (fun m -> m "error %a while listing store" ROStore.pp_error e) ;
        assert false
      | Ok files ->
        Lwt_list.fold_left_s (fun acc -> function
            | name, `Dictionary ->
              Logs.err (fun m -> m "got dictionary, expected value for %s" name) ;
              assert false
            | name, `Value ->
              ROStore.get store (Mirage_kv.Key.v name) >|= function
              | Error e ->
                Logs.err (fun m -> m "error %a while reading %s" ROStore.pp_error e name) ;
                assert false
              | Ok data -> (name, data) :: acc)
          [] files

  let load_git conduit resolver =
    load_zones conduit resolver >|= fun bindings ->
    Logs.info (fun m -> m "found %d bindings: %a" (List.length bindings)
                  Fmt.(list ~sep:(unit ",@ ") (pair ~sep:(unit ": ") string int))
                  (List.map (fun (k, v) -> k, String.length v) bindings)) ;
    List.fold_left (fun trie (k, data) ->
        match Udns_zonefile.load data with
        | Error msg ->
          Logs.err (fun m -> m "error while loading zonefile %s: %s" k msg) ;
          trie
        | Ok rrs ->
          let trie' = Udns_trie.insert_map rrs trie in
          match Udns_trie.check trie' with
          | Ok () -> trie'
          | Error e ->
            Logs.err (fun m -> m "error (while processing %s) %a during check()"
                         k Udns_trie.pp_err e) ;
            trie)
      Udns_trie.empty bindings

  module Store = Irmin_mirage.Git.KV_RW(Irmin_git.Mem)(P)

  let store_zone t zone =
    (* TODO maybe make this conditionally on modifications of the zone? *)
    match Udns_server.text zone (Udns_server.Primary.data t) with
    | Error msg ->
      Logs.err (fun m -> m "error while converting zone %a: %s" Domain_name.pp zone msg) ;
      assert false
    | Ok data ->
      (fun store -> Store.set store (Mirage_kv.Key.v (Domain_name.to_string zone)) data)

  let store_zones t resolver conduit =
    let data = Udns_server.Primary.data t in
    match
      Udns_trie.folde (Domain_name.root) Udns.Rr_map.Soa data
        (fun dname _ acc -> store_zone t dname :: acc) []
    with
    | Error e ->
      Logs.err (fun m -> m "error while folding while storing zones %a" Udns_trie.pp_e e) ;
      Lwt.return_unit
    | Ok actions ->
      Irmin_git.Mem.v (Fpath.v ".") >>= function
      | Error _ -> assert false
      | Ok git ->
        Store.connect git ~conduit ~resolver ~author:"primary DNS"
          ~msg:(fun _ -> "a zone change") () (Key_gen.remote ()) >>= fun store ->
        (* TODO removal of a zone should lead to dropping this zone from git! *)
        Store.batch store (fun s ->
            Lwt_list.fold_left_s
              (fun r f -> match r with Ok () -> f s | Error e -> Lwt.return (Error e))
              (Ok ()) actions) >|= function
        | Ok () -> Logs.app (fun m -> m "pushed zonefiles")
        | Error e ->
          (* TODO bail out!? try again? (https://github.com/mirage/ocaml-git/issues/342) *)
          Logs.err (fun m -> m "error while pushing zonefiles %a" Store.pp_write_error e)

  module D = Udns_mirage_server.Make(P)(M)(T)(S)

  let start _rng _pclock _mclock _time s resolver conduit _ =
    let keys = List.fold_left (fun acc str ->
        match Udns.Dnskey.name_key_of_string str with
        | Error (`Msg msg) -> Logs.err (fun m -> m "key parse error: %s" msg) ; acc
        | Ok (name, key) -> (name, key) :: acc)
        [] (Key_gen.keys ())
    in
    load_git conduit resolver >>= fun trie ->
    let on_update t = store_zones t resolver conduit in
    let on_notify _ t =
      load_git conduit resolver >|= fun trie ->
      Some trie
    in
    let t =
      Udns_server.Primary.create ~keys ~a:[Udns_server.Authentication.tsig_auth]
        ~tsig_verify:Udns_tsig.verify ~tsig_sign:Udns_tsig.sign
        ~rng:R.generate trie
    in
    D.primary ~on_update ~on_notify s t ;
    S.listen s
end
