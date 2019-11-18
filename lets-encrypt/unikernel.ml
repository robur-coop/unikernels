(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Mirage_types_lwt

open Lwt.Infix

open Dns

module Client (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S)= struct
  module Acme = Letsencrypt.Client.Make(Cohttp_mirage.Client)

  module D = Dns_mirage.Make(S)
  module DS = Dns_server_mirage.Make(P)(M)(T)(S)

  let gen_rsa seed =
    let seed = Cstruct.of_string seed in
    let g = Nocrypto.Rng.(create ~seed (module Generators.Fortuna)) in
    Nocrypto.Rsa.generate ~g 4096

  (* act as a hidden dns secondary and receive notifies, sweep through the zone for signing requests without corresponding (non-expired) certificate

     requires transfer and update keys

     on startup or when a notify is received, we fold over all TLSA records
      if there's a single csr and no valid and public-key matching cert get a cert from let's encrypt
      let's encrypt (http[s]) and dns challenge is used
      resulting certificate is nsupdated to primary dns

     Acme.initialise is done just at boottime

     then zone transfer and notifies is acted upon

     for each new tlsa record where selector = private and the content can be
       parsed as csr with a domain name we have keys for (or update uses the
       right key)
 *)

  let interesting_csr, interesting_cert =
    let tlsa_interesting t =
      t.Tlsa.matching_type = No_hash &&
      t.cert_usage = Domain_issued_certificate
    in
    ((fun t -> tlsa_interesting t && t.selector = Private),
     (fun t -> tlsa_interesting t && t.selector = Full_certificate))

  let valid_and_matches_csr csr cert =
    (* parse csr, parse cert: match public keys, match validity of cert *)
    match
      X509.Signing_request.decode_der csr.Tlsa.data,
      X509.Certificate.decode_der cert.Tlsa.data
    with
    | Ok csr, Ok cert ->
      let now_plus1 =
        let (days, ps) = P.now_d_ps () in
        Ptime.v (succ days, ps)
      in
      let _, until = X509.Certificate.validity cert in
      let csr_key = X509.Public_key.id X509.Signing_request.((info csr).public_key)
      and cert_key = X509.Public_key.id (X509.Certificate.public_key cert)
      in
      let valid = Ptime.is_later until ~than:now_plus1
      and key_eq = Cstruct.equal csr_key cert_key
      in
      begin match valid, key_eq with
        | true, true -> true
        | false, _ ->
          Logs.err (fun m -> m "%a is not later than %a"
                       (Ptime.pp_human ()) until (Ptime.pp_human ()) now_plus1) ;
          false
        | _, false ->
          Logs.err (fun m -> m "public keys do not match") ;
          false
      end
    | _ ->
      Logs.err (fun m -> m "couldn't parse csr or cert, returning false from matches") ;
      false

  let start _random _pclock _mclock _ stack res ctx _ =
    let update_keys, dns_keys =
      List.fold_left (fun (up, keys) str_key ->
          match Dnskey.name_key_of_string str_key with
          | Error (`Msg msg) -> invalid_arg ("couldn't parse dnskey: " ^ msg)
          | Ok (key_name, dnskey) ->
            Logs.app (fun m -> m "inserting key for %a" Domain_name.pp key_name) ;
            let up =
              (* TODO domain_name API *)
              if Astring.String.is_infix ~affix:"update" (Domain_name.to_string key_name) then
                let zone = Domain_name.drop_label_exn ~amount:2 key_name in
                Logs.app (fun m -> m "inserting zone %a update key" Domain_name.pp zone) ;
                Domain_name.Map.add zone (key_name, dnskey) up
              else
                up
            in
            (up, (key_name, dnskey) :: keys))
        (Domain_name.Map.empty, []) (Key_gen.dns_keys ())
    in
    let dns_server = Key_gen.dns_server () in
    (* TODO rework to use ip from transfer key! *)
    let dns_secondary =
      Dns_server.Secondary.create ~primary:dns_server
        ~a:[ Dns_server.Authentication.tsig_auth ]
        ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign
        ~rng:R.generate dns_keys
    in
    (* TODO check that we've for each zone (of the secondary) an update key *)

    (* we actually need to find the zones for which we have update keys
       (secondary logic cares about zone transfer key) *)
    let flow = ref None in
    let send_dns data =
      Logs.debug (fun m -> m "writing to %a" Ipaddr.V4.pp dns_server) ;
      let tcp = S.tcpv4 stack in
      let rec send again =
        match !flow with
        | None ->
          if again then
            S.TCPV4.create_connection tcp (dns_server, Key_gen.port ()) >>= function
            | Error e ->
              Logs.err (fun m -> m "failed to create connection to NS: %a" S.TCPV4.pp_error e) ;
              Lwt.return (Error (`Msg (Fmt.to_to_string S.TCPV4.pp_error e)))
            | Ok f -> flow := Some (D.of_flow f) ; send false
          else
            Lwt.return_error (`Msg "couldn't reach authoritative nameserver")
        | Some f ->
          D.send_tcp (D.flow f) data >>= function
          | Error () -> flow := None ; send (not again)
          | Ok () -> Lwt.return_ok ()
      in
      send true
    and recv_dns () =
      (* we expect a single reply! *)
      match !flow with
      | None -> Lwt.return_error (`Msg "no TCP flow")
      | Some f ->
        D.read_tcp f >|= function
        | Ok data -> Ok data
        | Error () -> Error (`Msg "error while reading from flow")
    in

    let mem_flight, add_flight, remove_flight =
      (* TODO use a map with number of attempts *)
      let in_flight = ref Domain_name.Set.empty in
      (fun x -> Domain_name.Set.mem x !in_flight),
      (fun n ->
         Logs.info (fun m -> m "adding %a to in_flight" Domain_name.pp n);
         in_flight := Domain_name.Set.add n !in_flight ;
         Logs.debug (fun m -> m "in_flight is %a"
                        Fmt.(list ~sep:(unit ",@ ") Domain_name.pp)
                        (Domain_name.Set.elements !in_flight))),
      (fun n ->
         Logs.info (fun m -> m "removing %a from in_flight" Domain_name.pp n);
         in_flight := Domain_name.Set.remove n !in_flight ;
         Logs.debug (fun m -> m "in_flight is %a"
                        Fmt.(list ~sep:(unit ",@ ") Domain_name.pp)
                        (Domain_name.Set.elements !in_flight)))
    in
    let request_certificate server le ctx ~tlsa_name ~keyname dnskey csr =
      if mem_flight tlsa_name then
        Logs.err (fun m -> m "request with %a already in-flight"
                     Domain_name.pp tlsa_name)
      else begin
        Logs.info (fun m -> m "running let's encrypt service for %a"
                      Domain_name.pp tlsa_name);
        add_flight tlsa_name;
        (* request new cert in async *)
        Lwt.async (fun () ->
            (* may get rid of it, once our AXFR with the challenge has been received by us, ask for the cert! :) *)
            let sleep () = OS.Time.sleep_ns (Duration.of_sec 3) in
            let now = Ptime.v (P.now_d_ps ()) in
            let id = Randomconv.int16 R.generate in
            let zone = Domain_name.host_exn @@ Domain_name.drop_label_exn ~amount:2 keyname in
            let solver = Letsencrypt.Client.default_dns_solver id now send_dns ~recv:recv_dns ~keyname dnskey ~zone in
            Acme.sign_certificate ~ctx ~solver le sleep csr >>= function
            | Error (`Msg e) ->
              Logs.err (fun m -> m "error %s while signing %a" e Domain_name.pp tlsa_name);
              remove_flight tlsa_name;
              Lwt.return_unit
            | Ok cert ->
              let certificate = X509.Certificate.encode_der cert in
              Logs.info (fun m -> m "certificate received for %a" Domain_name.pp tlsa_name);
              match Dns_trie.lookup tlsa_name Rr_map.Tlsa (Dns_server.Secondary.data server) with
              | Error e ->
                Logs.err (fun m -> m "lookup error for tlsa %a: %a (expected the signing request!)"
                             Domain_name.pp tlsa_name Dns_trie.pp_e e);
                remove_flight tlsa_name;
                Lwt.return_unit
              | Ok (ttl, tlsas) ->
                let update =
                  let add =
                    let tlsa = { Tlsa.cert_usage = Domain_issued_certificate ;
                                 selector = Full_certificate ;
                                 matching_type = No_hash ;
                                 data = certificate }
                    in
                    Packet.Update.Add Rr_map.(B (Tlsa, (3600l, Tlsa_set.singleton tlsa)))
                  and remove =
                    Rr_map.Tlsa_set.fold
                      (fun tlsa acc ->
                         if interesting_cert tlsa then
                           Packet.Update.Remove_single Rr_map.(B (Tlsa, (0l, Tlsa_set.singleton tlsa))) :: acc
                         else
                           acc)
                      tlsas []
                  in
                  let update = Domain_name.Map.singleton tlsa_name (remove @ [ add ]) in
                  (Domain_name.Map.empty, update)
                and zone = Packet.Question.create zone Rr_map.Soa
                and header = (Randomconv.int16 R.generate, Packet.Flags.empty)
                in
                let packet = Packet.create header zone (`Update update) in
                match Dns_tsig.encode_and_sign ~proto:`Tcp packet now dnskey keyname with
                | Error s ->
                  remove_flight tlsa_name;
                  Logs.err (fun m -> m "Error %a while encoding and signing %a" Dns_tsig.pp_s s Domain_name.pp tlsa_name);
                  Lwt.return_unit
                | Ok (data, mac) ->
                  send_dns data >>= function
                  | Error (`Msg e) ->
                    (* TODO: should retry DNS send *)
                    remove_flight tlsa_name;
                    Logs.err (fun m -> m "error %s while sending nsupdate %a" e Domain_name.pp tlsa_name);
                    Lwt.return_unit
                  | Ok () ->
                    recv_dns () >|= function
                    | Error (`Msg e) ->
                      (* TODO: should retry DNS send *)
                      remove_flight tlsa_name;
                      Logs.err (fun m -> m "error %s while reading DNS %a" e Domain_name.pp tlsa_name)
                    | Ok data ->
                      remove_flight tlsa_name;
                      match Dns_tsig.decode_and_verify now dnskey keyname ~mac data with
                      | Error e ->
                        Logs.err (fun m -> m "error %a while decoding nsupdate answer %a"
                                     Dns_tsig.pp_e e Domain_name.pp tlsa_name)
                      | Ok (res, _, _) ->
                        match Packet.reply_matches_request ~request:packet res with
                        | Ok _ -> ()
                        | Error e ->
                          (* TODO: if badtime, adjust our time (to the other time) and resend ;) *)
                          Logs.err (fun m -> m "invalid reply %a for %a, got %a"
                                       Packet.pp_mismatch e Packet.pp packet Packet.pp res))
      end
    in

    let account_key = gen_rsa (Key_gen.account_key_seed ()) in
    Conduit_mirage.with_tls ctx >>= fun ctx ->
    let ctx = Cohttp_mirage.Client.ctx res ctx in
    let directory =
      if Key_gen.production () then begin
        Logs.warn (fun m -> m "production environment - take care what you do");
        Letsencrypt.letsencrypt_url
      end else begin
        Logs.warn (fun m -> m "staging environment - test use only");
        Letsencrypt.letsencrypt_staging_url
      end
    in
    Acme.initialise ~ctx ~directory account_key >>= function
    | Error (`Msg e) -> Logs.err (fun m -> m "error %s" e) ; Lwt.return_unit
    | Ok le ->
      Logs.info (fun m -> m "initialised lets encrypt");

      let on_update ~old:_ t =
        (* what to do here?
             foreach _changed_ TLSA record (can as well just do all for now)
             - if it starts with _letsencrypt._tcp (needs domain_name API)
             - if we have an update key for this zone
             - look whether there's 1 CSR and 0 CERT, and act
           --> can be achieved by fold, but how to extract "update key for zone"?
             -> data structure update_keys is a map zone -> (keyname, dnskey)
             -> for a TLSA (_letsencrypt._tcp.<host>):
               -> let zone = drop two labels
               -> lookup zone in update_keys, rinse repeat with zone dropping labels *)
        let name_of_interest name =
          (* TODO domain_name API *)
          let len = Domain_name.count_labels name in
          if len < 2 then
            false
          else
            let first = Domain_name.get_label_exn name 0
            and second = Domain_name.get_label_exn name 1
            in
            Domain_name.(equal_label first "_letsencrypt" && equal_label second "_tcp")
        and find_update_key name =
          (* not clear whether this is worth it - the recursion below would as well drop them *)
          let base = Domain_name.drop_label ~amount:2 name in
          let rec find_key = function
            | Error (`Msg msg) ->
              Logs.err (fun m -> m "find_key for %a failed with %s" Domain_name.pp name msg);
              None
            | Ok name ->
              match Domain_name.Map.find name update_keys with
              | None -> find_key (Domain_name.drop_label name)
              | Some key -> Some key
          in
          find_key base
        in
        let trie = Dns_server.Secondary.data t in
        Dns_trie.fold Dns.Rr_map.Tlsa trie
          (fun name (_, tlsas) () ->
             (* name of interest? *)
             if name_of_interest name then
               let csrs = Rr_map.Tlsa_set.filter interesting_csr tlsas
               and certs = Rr_map.Tlsa_set.filter interesting_cert tlsas
               in
               if Rr_map.Tlsa_set.cardinal csrs <> 1 then
                 Logs.warn (fun m -> m "no or multiple signing requests found for %a (skipping)"
                               Domain_name.pp name)
               else begin
                 let csr = Rr_map.Tlsa_set.choose csrs in
                 let interesting = match Rr_map.Tlsa_set.cardinal certs with
                   | 0 ->
                     Logs.warn (fun m -> m "no certificate found for %a, requesting"
                                   Domain_name.pp name);
                     true
                   | 1 ->
                     let cert = Rr_map.Tlsa_set.choose certs in
                     if valid_and_matches_csr csr cert then begin
                       Logs.debug (fun m -> m "certificate already exists for signing request %a, skipping"
                                      Domain_name.pp name);
                       false
                     end else begin
                       Logs.warn (fun m -> m "certificate not valid or doesn't match signing request %a, requesting"
                                     Domain_name.pp name);
                       true
                     end
                   | _ ->
                     Logs.err (fun m -> m "multiple certificates found for %a, skipping"
                                  Domain_name.pp name);
                     false
                 in
                 if interesting then
                   (* update key exists? *)
                   match find_update_key name with
                   | None ->
                     Logs.err (fun m -> m "couldn't find an update key for %a"
                                  Domain_name.pp name)
                   | Some (keyname, key) ->
                     match X509.Signing_request.decode_der csr.Tlsa.data with
                     | Error (`Msg str) -> Logs.err (fun m -> m "couldn't parse signing request: %s" str)
                     | Ok csr ->
                       match X509.(Distinguished_name.common_name Signing_request.((info csr).subject)) with
                       | None -> Logs.err (fun m -> m "cannot find name of signing request")
                       | Some nam ->
                         begin match Domain_name.of_string nam with
                           | Error (`Msg msg) -> Logs.err (fun m -> m "error %s while creating domain name of %s" msg nam)
                           | Ok csr_name ->
                             if not (Domain_name.is_subdomain ~domain:csr_name ~subdomain:name) then
                               Logs.err (fun m -> m "csr cn %a isn't a superdomain of DNS %a"
                                            Domain_name.pp csr_name Domain_name.pp name)
                             else
                               request_certificate t le ctx ~tlsa_name:name ~keyname key csr
                         end
                 else
                   Logs.warn (fun m -> m "not interesting (certs) %a" Domain_name.pp name)
               end
             else
               Logs.warn (fun m -> m "name not interesting %a" Domain_name.pp name)) ();
        Lwt.return_unit
      in
      DS.secondary ~on_update stack dns_secondary ;
      S.listen stack
end
