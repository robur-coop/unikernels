(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Dns

module Client (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Mirage_stack.V4) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S)= struct
  module Acme = Letsencrypt.Client.Make(Cohttp_mirage.Client)

  module D = Dns_mirage.Make(S)
  module DS = Dns_server_mirage.Make(P)(M)(T)(S)

  let gen_rsa seed =
    let seed = Cstruct.of_string seed in
    let g = Mirage_crypto_rng.(create ~seed (module Mirage_crypto_rng.Fortuna)) in
    Mirage_crypto_pk.Rsa.generate ~g ~bits:4096 ()

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

  let valid_and_matches_csr csr cert =
    (* parse csr, parse cert: match public keys, match validity of cert *)
    match
      X509.Signing_request.decode_der csr.Tlsa.data,
      X509.Certificate.decode_der cert.Tlsa.data
    with
    | Ok csr, Ok cert ->
      let now_plus_two_weeks =
        let (days, ps) = P.now_d_ps () in
        Ptime.v (days + 14, ps)
      and now = Ptime.v (P.now_d_ps ())
      in
      if Dns_certify.cert_matches_csr ~until:now_plus_two_weeks now csr cert then
        None
      else
        Some csr
    | Ok csr, Error `Msg e ->
      Logs.err (fun m -> m "couldn't parse certificate %s, requesting new one" e);
      Some csr
    | Error `Msg e, _ ->
      Logs.err (fun m -> m "couldn't parse csr %s, nothing to see here" e) ;
      None

  let contains_csr_without_certificate name tlsas =
    let csrs = Rr_map.Tlsa_set.filter Dns_certify.is_csr tlsas in
    if Rr_map.Tlsa_set.cardinal csrs <> 1 then begin
      Logs.warn (fun m -> m "no or multiple signing requests found for %a (skipping)"
                    Domain_name.pp name);
      None
    end else
      let csr = Rr_map.Tlsa_set.choose csrs in
      let certs = Rr_map.Tlsa_set.filter Dns_certify.is_certificate tlsas in
      match Rr_map.Tlsa_set.cardinal certs with
      | 0 ->
        Logs.warn (fun m -> m "no certificate found for %a, requesting"
                      Domain_name.pp name);
        begin match X509.Signing_request.decode_der csr.Tlsa.data with
          | Ok csr -> Some csr
          | Error `Msg e ->
            Logs.warn (fun m -> m "couldn't parse CSR %s" e);
            None
        end
      | 1 ->
        begin
          let cert = Rr_map.Tlsa_set.choose certs in
          match valid_and_matches_csr csr cert with
          | None ->
            Logs.debug (fun m -> m "certificate already exists for signing request %a, skipping"
                           Domain_name.pp name);
            None
          | Some csr ->
            Logs.warn (fun m -> m "certificate not valid or doesn't match signing request %a, requesting"
                          Domain_name.pp name);
            Some csr
        end
      | _ ->
        Logs.err (fun m -> m "multiple certificates found for %a, skipping"
                     Domain_name.pp name);
        None

  let start _random _pclock _mclock _ stack res ctx =
    let keyname, keyzone, dnskey =
      match Dnskey.name_key_of_string (Key_gen.dns_key ()) with
      | Error (`Msg msg) -> Logs.err (fun m -> m "couldn't parse dnskey: %s" msg) ; exit 64
      | Ok (keyname, dnskey) ->
        match Domain_name.find_label keyname (function "_update" -> true | _ -> false) with
        | None -> Logs.err (fun m -> m "dnskey is not an update key") ; exit 64
        | Some idx ->
          let amount = succ idx in
          let zone = Domain_name.(host_exn (drop_label_exn ~amount keyname)) in
          Logs.app (fun m -> m "using key %a for zone %a" Domain_name.pp keyname Domain_name.pp zone);
          keyname, zone, dnskey
    in
    let dns_server = Key_gen.dns_server () in
    let dns_state = ref
        (Dns_server.Secondary.create ~primary:dns_server ~rng:R.generate
           ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign [ keyname, dnskey ])
    in
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
    let request_certificate server le ctx ~tlsa_name csr =
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
            let sleep n = T.sleep_ns (Duration.of_sec n) in
            let now () = Ptime.v (P.now_d_ps ()) in
            let id = Randomconv.int16 R.generate in
            let solver = Letsencrypt.Client.nsupdate ~proto:`Tcp id now send_dns ~recv:recv_dns ~zone:keyzone ~keyname dnskey in
            Acme.sign_certificate ~ctx solver le sleep csr >>= function
            | Error (`Msg e) ->
              Logs.err (fun m -> m "error %s while signing %a" e Domain_name.pp tlsa_name);
              remove_flight tlsa_name;
              Lwt.return_unit
            | Ok [] ->
              Logs.err (fun m -> m "received an empty certificate chain for %a" Domain_name.pp tlsa_name);
              Lwt.return_unit
            | Ok (cert::cas) ->
              Logs.info (fun m -> m "certificate received for %a" Domain_name.pp tlsa_name);
              match Dns_trie.lookup tlsa_name Rr_map.Tlsa (Dns_server.Secondary.data server) with
              | Error e ->
                Logs.err (fun m -> m "lookup error for tlsa %a: %a (expected the signing request!)"
                             Domain_name.pp tlsa_name Dns_trie.pp_e e);
                remove_flight tlsa_name;
                Lwt.return_unit
              | Ok (_, tlsas) ->
                (* from tlsas, we need to remove the end entity certificates *)
                (* also potentially all CAs that are not part of cas *)
                (* we should add the new certificate and potentially CAs *)
                let cas' = List.map X509.Certificate.encode_der cas in
                let to_remove, cas_not_to_add =
                  Rr_map.Tlsa_set.fold (fun tlsa (to_rm, not_to_add) ->
                      if Dns_certify.is_ca_certificate tlsa then
                        if List.mem tlsa.Tlsa.data cas' then
                          to_rm, tlsa.Tlsa.data :: not_to_add
                        else
                          tlsa :: to_rm, not_to_add
                      else if Dns_certify.is_certificate tlsa then
                        tlsa :: to_rm, not_to_add
                      else
                        to_rm, not_to_add)
                    tlsas ([], [])
                in
                let update =
                  let add =
                    let tlsas =
                      let cas_to_add =
                        List.filter (fun ca -> not (List.mem ca cas_not_to_add)) cas'
                      in
                      let cas = List.map Dns_certify.ca_certificate cas_to_add in
                      Rr_map.Tlsa_set.of_list (Dns_certify.certificate cert :: cas)
                    in
                    Packet.Update.Add Rr_map.(B (Tlsa, (3600l, tlsas)))
                  and remove =
                    List.map (fun tlsa ->
                        Packet.Update.Remove_single Rr_map.(B (Tlsa, (0l, Tlsa_set.singleton tlsa))))
                      to_remove
                  in
                  let update = Domain_name.Map.singleton tlsa_name (remove @ [ add ]) in
                  (Domain_name.Map.empty, update)
                and zone = Packet.Question.create keyzone Rr_map.Soa
                and header = (Randomconv.int16 R.generate, Packet.Flags.empty)
                in
                let packet = Packet.create header zone (`Update update) in
                match Dns_tsig.encode_and_sign ~proto:`Tcp packet (now ()) dnskey keyname with
                | Error s ->
                  remove_flight tlsa_name;
                  Logs.err (fun m -> m "Error %a while encoding and signing %a"
                               Dns_tsig.pp_s s Domain_name.pp tlsa_name);
                  Lwt.return_unit
                | Ok (data, mac) ->
                  send_dns data >>= function
                  | Error (`Msg e) ->
                    (* TODO: should retry DNS send *)
                    remove_flight tlsa_name;
                    Logs.err (fun m -> m "error %s while sending nsupdate %a"
                                 e Domain_name.pp tlsa_name);
                    Lwt.return_unit
                  | Ok () ->
                    recv_dns () >|= function
                    | Error (`Msg e) ->
                      (* TODO: should retry DNS send *)
                      remove_flight tlsa_name;
                      Logs.err (fun m -> m "error %s while reading DNS %a"
                                   e Domain_name.pp tlsa_name)
                    | Ok data ->
                      remove_flight tlsa_name;
                      match Dns_tsig.decode_and_verify (now ()) dnskey keyname ~mac data with
                      | Error e ->
                        Logs.err (fun m -> m "error %a while decoding nsupdate answer %a"
                                     Dns_tsig.pp_e e Domain_name.pp tlsa_name)
                      | Ok (res, _, _) ->
                        match Packet.reply_matches_request ~request:packet res with
                        | Ok _ -> ()
                        | Error e ->
                          (* TODO: if badtime, adjust our time (to the other time) and resend ;) *)
                          Logs.err (fun m -> m "invalid reply %a for %a, got %a"
                                       Packet.pp_mismatch e Packet.pp packet
                                       Packet.pp res))
      end
    in

    let account_key = gen_rsa (Key_gen.account_key_seed ()) in
    Conduit_mirage.with_tls ctx >>= fun ctx ->
    let ctx = Cohttp_mirage.Client.ctx res ctx in
    let endpoint =
      if Key_gen.production () then begin
        Logs.warn (fun m -> m "production environment - take care what you do");
        Letsencrypt.letsencrypt_production_url
      end else begin
        Logs.warn (fun m -> m "staging environment - test use only");
        Letsencrypt.letsencrypt_staging_url
      end
    in
    let email = Key_gen.email () in
    Acme.initialise ~ctx ~endpoint ?email account_key >>= function
    | Error (`Msg e) -> Logs.err (fun m -> m "error %s" e) ; Lwt.return_unit
    | Ok le ->
      Logs.info (fun m -> m "initialised lets encrypt");

      let on_update ~old:_ t =
        dns_state := t;
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
        let trie = Dns_server.Secondary.data t in
        Dns_trie.fold Dns.Rr_map.Tlsa trie
          (fun name (_, tlsas) () ->
             if Dns_certify.is_name name then
               match contains_csr_without_certificate name tlsas with
               | None -> Logs.debug (fun m -> m "not interesting (does not contain CSR without valid certificate) %a" Domain_name.pp name)
               | Some csr -> request_certificate t le ctx ~tlsa_name:name csr
             else
               Logs.debug (fun m -> m "name not interesting %a" Domain_name.pp name)) ();
        Lwt.return_unit
      in
      Lwt.async (fun () ->
          T.sleep_ns (Duration.of_day 1) >>= fun () ->
          on_update ~old:(Dns_server.Secondary.data !dns_state) !dns_state);
      DS.secondary ~on_update stack !dns_state ;
      S.listen stack
end
