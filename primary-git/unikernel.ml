(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) = struct

  module Store = Irmin_mirage.Git.Mem.KV(Irmin.Contents.String)
  module Sync = Irmin.Sync(Store)

  let connect_store resolver conduit =
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >|= fun repo ->
    repo, Store.remote ~conduit ~resolver (Key_gen.remote ())

  let pull_store repo upstream =
    Logs.info (fun m -> m "pulling from remote!");
    Sync.pull repo upstream `Set >|= function
    | Ok () -> Logs.info (fun m -> m "ok, pulled!")
    | Error (#Sync.fetch_error as e) -> Logs.warn (fun m -> m "fetch error %a" Sync.pp_fetch_error e)
    | Error (`Conflict msg) -> Logs.warn (fun m -> m "pull failed with conflict %s" msg)

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
      let parse_and_maybe_add trie zone data =
        Udns_zone.parse data >>= fun rrs ->
        if not (Domain_name.Map.for_all (fun name _ -> Domain_name.sub ~domain:zone ~subdomain:name) rrs) then
          Error (`Msg (Fmt.strf "an entry of %a is not in its zone, won't handle this@.%a"
                         Domain_name.pp zone Udns.Name_rr_map.pp rrs))
        else
          let trie' = Udns_trie.insert_map rrs trie in
          let err_to_str =
            function Ok x -> Ok x | Error e -> Error (`Msg (Fmt.to_to_string Udns_trie.pp_err e))
          in
          err_to_str (Udns_trie.check trie') >>= fun () ->
          (* this prints all zones of the trie' *)
          let str_to_msg = function Ok x -> Ok x | Error e -> Error (`Msg e) in
          str_to_msg (Udns_server.text zone trie') >>| fun zone_data ->
          Logs.info (fun m -> m "loade zone %a" Domain_name.pp zone);
          Logs.info (fun m -> m "loaded zone@.%s" zone_data);
          trie'
      in
      List.fold_left (fun acc (zone, data) ->
          acc >>= fun trie ->
          parse_and_maybe_add trie zone data)
        (Ok Udns_trie.empty) bindings

  let store_zone t store zone =
    (* TODO maybe make this conditionally on modifications of the zone? *)
    match Udns_server.text zone (Udns_server.Primary.data t) with
    | Error msg ->
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
    let data = Udns_server.Primary.data t in
    let zones =
      Udns_trie.fold Udns.Rr_map.Soa data
        (fun dname soa acc ->
           match Udns_trie.lookup dname Soa old with
           | Ok old when Udns.Soa.newer ~old soa -> dname :: acc
           | Ok _ -> acc
           | Error _ -> dname :: acc)
        []
    in
    Lwt_list.iter_s (store_zone t store) zones >>= fun () ->
    (* TODO removal of a zone should lead to dropping this zone from git! *)
    Logs.info (fun m -> m "pushing to remote!");
    Sync.push store upstream  >|= function
    | Ok () -> Logs.app (fun m -> m "pushed zonefiles")
    | Error `Detached_head -> Logs.err (fun m -> m "detached head while pushing")
    | Error `No_head -> Logs.err (fun m -> m "no head while pushing")
    | Error `Not_available -> Logs.err (fun m -> m "not available while pushing")
    | Error (`Msg msg) -> Logs.err (fun m -> m "pushing: %s" msg)

  module D = Udns_mirage_server.Make(P)(M)(T)(S)

  let start _rng _pclock _mclock _time s resolver conduit _ =
    let keys = List.fold_left (fun acc str ->
        match Udns.Dnskey.name_key_of_string str with
        | Error (`Msg msg) -> Logs.err (fun m -> m "key parse error: %s" msg) ; acc
        | Ok (name, key) -> (name, key) :: acc)
        [] (Key_gen.keys ())
    in
    connect_store resolver conduit >>= fun (store, upstream) ->
    Logs.info (fun m -> m "i have now master!");
    load_git store upstream >>= function
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error during loading git %s" msg);
      Lwt.return_unit
    | Ok trie ->
      let on_update ~old t = store_zones ~old t store upstream in
      let on_notify n t =
        (match n with
        | `Notify soa ->
          Logs.err (fun m -> m "ignoring normal notify %a" Fmt.(option ~none:(unit "no soa") Udns.Soa.pp) soa)
        | `Signed_notify soa ->
          Logs.info (fun m -> m "got notified, checking out %a" Fmt.(option ~none:(unit "no soa") Udns.Soa.pp) soa));
        load_git store upstream >|= function
        | Error msg ->
          Logs.err (fun m -> m "error while loading git while in notify, continuing with old data");
          None
        | Ok trie ->
          Logs.info (fun m -> m "loaded a new trie from git!");
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
