(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Mirage_stack.V4) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) = struct

  module Store = Irmin_mirage_git.Mem.KV(Irmin.Contents.String)
  module Sync = Irmin.Sync(Store)

  let connect_store resolver conduit =
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >|= fun repo ->
    repo, Store.remote ~conduit ~resolver (Key_gen.remote ())

  let pull_store repo upstream =
    Logs.info (fun m -> m "pulling from remote!");
    Sync.pull repo upstream `Set >|= function
    | Ok `Empty -> Logs.warn (fun m -> m "pulled empty repository")
    | Ok (`Head _ as s) -> Logs.info (fun m -> m "ok, pulled %a!" Sync.pp_status s)
    | Error (`Msg e) -> Logs.warn (fun m -> m "pull error %s" e)
    | Error (`Conflict msg) -> Logs.warn (fun m -> m "pull conflict %s" msg)

  let load_zones store upstream =
    (* TODO recurse over directories (io/nqsb, org/openmirage) for zones *)
    pull_store store upstream >>= fun () ->
    Store.list store [] >>= fun files ->
    Lwt_list.fold_left_s (fun acc (name, kind) ->
        match acc with
        | Error e -> Lwt.return (Error e)
        | Ok acc -> match kind, Domain_name.of_string name with
          | `Node, _ -> Lwt.return (Error (name, "got node, expected contents"))
          | `Contents, Error (`Msg e) -> Lwt.return (Error (name, "not a domain name " ^ e))
          | `Contents, Ok zone ->
            (* TODO directory traversal for zones *)
            Store.get store [name] >|= fun data ->
            Ok ((zone, data) :: acc))
      (Ok []) files

  let load_git store upstream =
    load_zones store upstream >|= function
    | Error (ctx, e) -> Error (`Msg ("while loading zones from git: " ^ ctx ^ " " ^ e))
    | Ok bindings ->
      Logs.info (fun m -> m "found %d bindings: %a" (List.length bindings)
                    Fmt.(list ~sep:(unit ",@ ") (pair ~sep:(unit ": ") Domain_name.pp int))
                    (List.map (fun (k, v) -> k, String.length v) bindings)) ;
      let open Rresult.R.Infix in
      (* split into keys and zones *)
      let keys, data =
        let is_key subdomain =
          Domain_name.is_subdomain ~domain:(Domain_name.of_string_exn "_keys") ~subdomain
        in
        let keys, data = List.partition (fun (name, _) -> is_key name) bindings in
        List.map (fun (n, v) -> Domain_name.drop_label_exn ~rev:true n, v) keys,
        data
      in
      let parse_and_maybe_add trie zone data =
        Logs.info (fun m -> m "parsing %a: %s" Domain_name.pp zone data);
        Dns_zone.parse data >>= fun rrs ->
        let tst subdomain = Domain_name.is_subdomain ~domain:zone ~subdomain in
        if not (Domain_name.Map.for_all (fun name _ -> tst name) rrs) then
          (* how does this behave in respect to delegation and glue?
             should be fine, if glue is needed it'll be a subdomain of zone *)
          Error (`Msg (Fmt.strf "an entry of %a is not in its zone, won't handle this@.%a"
                         Domain_name.pp zone Dns.Name_rr_map.pp rrs))
        else
          let trie' = Dns_trie.insert_map rrs trie in
          Rresult.R.reword_error
            (fun e -> `Msg (Fmt.to_to_string Dns_trie.pp_zone_check e))
            (Dns_trie.check trie') >>= fun () ->
          (* this prints all zones of the trie' *)
          Dns_server.text zone trie' >>| fun zone_data ->
          Logs.info (fun m -> m "loade zone %a" Domain_name.pp zone);
          Logs.info (fun m -> m "loaded zone@.%s" zone_data);
          trie'
      in
      List.fold_left (fun acc (zone, data) ->
          acc >>= fun trie ->
          parse_and_maybe_add trie zone data)
        (Ok Dns_trie.empty) data >>= fun data_trie ->
      let parse_keys zone keys =
        Dns_zone.parse keys >>= fun rrs ->
        let tst subdomain = Domain_name.is_subdomain ~domain:zone ~subdomain in
        if not (Domain_name.Map.for_all (fun name _ -> tst name) rrs) then
          Error (`Msg "key name not in zone")
        else
          (* from the name_rr_map extract a (Domain_name.t * Dnskey) list *)
          let keys =
            Domain_name.Map.fold (fun n data acc ->
                match Dns.Rr_map.(find Dnskey data) with
                | None ->
                  Logs.warn (fun m -> m "no dnskey found %a" Domain_name.pp n);
                  acc
                | Some (_, keys) ->
                  match Dns.Rr_map.Dnskey_set.elements keys with
                  | [ x ] ->
                    Logs.info (fun m -> m "inserting dnskey %a" Domain_name.pp n);
                    (n, x) :: acc
                  | xs ->
                    Logs.warn (fun m -> m "%d dnskeys for %a (expected exactly one)"
                                  (List.length xs) Domain_name.pp n);
                    acc)
              rrs []
          in
          Ok keys
      in
      List.fold_left (fun acc (zone, keys) ->
          acc >>= fun acc ->
          parse_keys zone keys >>| fun ks ->
          (ks @ acc))
        (Ok []) keys >>| fun keys ->
      (data_trie, keys)

  let store_zone t store zone =
    (* TODO maybe make this conditionally on modifications of the zone? *)
    match Dns_server.text zone (Dns_server.Primary.data t) with
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error while converting zone %a: %s" Domain_name.pp zone msg) ;
      Lwt.return_unit
    | Ok data ->
      let info = fun () ->
        let date = Int64.of_float Ptime.Span.(to_float_s (v (P.now_d_ps ()))) in
        let commit = Fmt.strf "change of %a" Domain_name.pp zone in
        Irmin.Info.v ~date ~author:"dns primary git server" commit
      in
      (* TODO to_strings once we use directories ;) *)
      Store.set ~info store [Domain_name.to_string zone] data >|= function
      | Ok () -> ()
      | Error _ -> Logs.err (fun m -> m "error while writing to store")

  let store_zones ~old t store upstream =
    let data = Dns_server.Primary.data t in
    let zones =
      Dns_trie.fold Dns.Rr_map.Soa data
        (fun dname soa acc ->
           match Dns_trie.lookup dname Dns.Rr_map.Soa old with
           | Ok old when Dns.Soa.newer ~old soa -> dname :: acc
           | Ok _ -> acc
           | Error _ -> dname :: acc)
        []
    in
    Lwt_list.iter_s (store_zone t store) zones >>= fun () ->
    (* TODO removal of a zone should lead to dropping this zone from git! *)
    Logs.info (fun m -> m "pushing to remote!");
    Sync.push store upstream  >|= function
    | Ok `Empty -> Logs.warn (fun m -> m "pushed empty zonefiles")
    | Ok (`Head _ as s) -> Logs.info (fun m -> m "pushed zonefile commit %a" Sync.pp_status s)
    | Error pe -> Logs.err (fun m -> m "push error %a" Sync.pp_push_error pe)

  module D = Dns_server_mirage.Make(P)(M)(T)(S)

  let start _rng _pclock _mclock _time s resolver conduit _ =
    connect_store resolver conduit >>= fun (store, upstream) ->
    Logs.info (fun m -> m "i have now master!");
    load_git store upstream >>= function
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error during loading git %s" msg);
      Lwt.return_unit
    | Ok (trie, keys) ->
      let on_update ~old t = store_zones ~old t store upstream in
      let on_notify n _t =
        match n with
        | `Notify soa ->
          Logs.err (fun m -> m "ignoring normal notify %a" Fmt.(option ~none:(unit "no soa") Dns.Soa.pp) soa);
          Lwt.return None
        | `Signed_notify soa ->
          Logs.info (fun m -> m "got notified, checking out %a" Fmt.(option ~none:(unit "no soa") Dns.Soa.pp) soa);
          load_git store upstream >|= function
          | Error (`Msg msg) ->
            Logs.err (fun m -> m "error %s while loading git while in notify, continuing with old data" msg);
            None
          | Ok trie ->
            Logs.info (fun m -> m "loaded a new trie from git!");
            Some trie
      in
      let t =
        Dns_server.Primary.create ~keys ~a:[Dns_server.Authentication.tsig_auth]
          ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign
          ~rng:R.generate trie
      in
      D.primary ~on_update ~on_notify s t ;
      S.listen s
end
