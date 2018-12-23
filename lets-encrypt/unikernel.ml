(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Mirage_types_lwt

open Lwt.Infix

module Client (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S)= struct
  module Acme = Letsencrypt.Client.Make (Cohttp_mirage.Client)

  module Dns = Dns_mirage.Make(S)

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
      t.Dns_packet.tlsa_matching_type = Dns_enum.Tlsa_no_hash &&
      t.Dns_packet.tlsa_cert_usage = Dns_enum.Domain_issued_certificate
    in
    ((fun t -> tlsa_interesting t && t.Dns_packet.tlsa_selector = Dns_enum.Tlsa_selector_private),
     (fun t -> tlsa_interesting t && t.Dns_packet.tlsa_selector = Dns_enum.Tlsa_full_certificate))

  let valid_and_matches_csr pclock csr cert =
    Logs.info (fun m -> m "does csr and cert match?") ;
    (* parse csr, parse cert: match public keys, match validity of cert *)
    match
      X509.Encoding.parse_signing_request csr.Dns_packet.tlsa_data,
      X509.Encoding.parse cert.Dns_packet.tlsa_data
    with
    | Some csr, Some cert ->
      Logs.info (fun m -> m "was able to parse certificate and csr") ;
      let now_plus1 =
        let (days, ps) = P.now_d_ps pclock in
        Ptime.v (succ days, ps)
      in
      let _, until = X509.validity cert in
      let csr_key = X509.key_id (X509.CA.info csr).X509.CA.public_key
      and cert_key = X509.key_id (X509.public_key cert)
      in
      Logs.info (fun m -> m "is %a later than %a? is %a equal to %a?"
                    (Ptime.pp_human ()) until (Ptime.pp_human ()) now_plus1
                    Cstruct.hexdump_pp csr_key Cstruct.hexdump_pp cert_key) ;
      Ptime.is_later until ~than:now_plus1 &&
      Cstruct.equal csr_key cert_key
    | _ ->
      Logs.err (fun m -> m "couldn't parse csr or cert, returning false from matches") ;
      false

  module Dns_server = Dns_mirage_server.Make(P)(M)(T)(S)

  let start _random pclock mclock _ stack res ctx _ =
    let update_keys, dns_keys =
      List.fold_left (fun (up, keys) str_key ->
          match Astring.String.cut ~sep:":" str_key with
          | None -> invalid_arg "couldn't parse dnskey"
          | Some (name, key) ->
            match Domain_name.of_string ~hostname:false name, Dns_packet.dnskey_of_string key with
            | Error _, _ | _, None -> invalid_arg "failed to parse dnskey"
            | Ok key_name, Some dnskey ->
              Logs.app (fun m -> m "inserting key for %a" Domain_name.pp key_name) ;
              let up =
                if Astring.String.is_infix ~affix:"update" name then
                  let zone = Domain_name.drop_labels_exn ~amount:2 key_name in
                  Logs.app (fun m -> m "inserting zone %a update key" Domain_name.pp zone) ;
                  Domain_name.Map.add zone (key_name, dnskey) up
                else
                  up
              in
              (up, (key_name, dnskey) :: keys))
        (Domain_name.Map.empty, []) (Key_gen.dns_keys ())
    in
    let dns_server = Key_gen.dns_server () in
    let dns_secondary =
      UDns_server.Secondary.create ~a:[ UDns_server.Authentication.tsig_auth ]
        ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign
        ~primary:dns_server ~rng:R.generate dns_keys
    in
    (* TODO check that we've for each zone (of the secondary) an update key *)

    (* we actually need to find the zones for which we have update keys
       (secondary logic cares about zone transfer key) *)
    let flow = ref None in
    let send_dns data =
      Logs.debug (fun m -> m "writing to %a" Ipaddr.V4.pp_hum dns_server) ;
      let tcp = S.tcpv4 stack in
      let rec send again =
        match !flow with
        | None ->
          if again then
            S.TCPV4.create_connection tcp (dns_server, 53) >>= function
            | Error e ->
              Logs.err (fun m -> m "failed to create connection to NS: %a" S.TCPV4.pp_error e) ;
              Lwt.return (Error (Fmt.to_to_string S.TCPV4.pp_error e))
            | Ok f -> flow := Some (Dns.of_flow f) ; send false
          else
            Lwt.return_error "couldn't reach authoritative nameserver"
        | Some f ->
          Dns.send_tcp (Dns.flow f) data >>= function
          | Error () -> flow := None ; send (not again)
          | Ok () -> Lwt.return_ok ()
      in
      send true
    and recv_dns () =
      (* we expect a single reply! *)
      match !flow with
      | None -> Lwt.return_error "no TCP flow"
      | Some f ->
        Dns.read_tcp f >|= function
        | Ok data -> Ok data
        | Error () -> Error "error while reading from flow"
    in

    (* TODO use a map with number of attempts *)
    let in_flight = ref Domain_name.Set.empty in
    let request_certificate server le ctx zone name (key_name, dnskey) csr =
      if Domain_name.Set.mem name !in_flight then
        Logs.err (fun m -> m "request with %a already in-flight" Domain_name.pp name)
      else begin
        Logs.info (fun m -> m "running let's encrypt service for %a, adding to in_flight %a" Domain_name.pp name Fmt.(list ~sep:(unit ",@ ") Domain_name.pp) (Domain_name.Set.elements !in_flight)) ;
        in_flight := Domain_name.Set.add name !in_flight ;
        (* request new cert in async *)
        Lwt.async (fun () ->
            let sleep () = OS.Time.sleep_ns (Duration.of_sec 3) in
            let now = Ptime.v (P.now_d_ps pclock) in
            let id = Randomconv.int16 R.generate in
            let solver = Letsencrypt.Client.default_dns_solver id now send_dns ~recv:recv_dns key_name dnskey in
            Acme.sign_certificate ~ctx ~solver le sleep csr >>= function
            | Error e ->
              Logs.err (fun m -> m "error %s, removing %a from in_flight" e Domain_name.pp name) ;
              in_flight := Domain_name.Set.remove name !in_flight ;
              Lwt.return_unit
            | Ok cert ->
              let certificate = X509.Encoding.cs_of_cert cert in
              Logs.info (fun m -> m "certificate received for %a" Domain_name.pp name) ;
              Logs.info (fun m -> m "der is %a" Cstruct.hexdump_pp certificate);
              match Dns_trie.lookup name Dns_map.Tlsa (UDns_server.Secondary.data server) with
              | Error e ->
                Logs.err (fun m -> m "couldn't find tlsa for %a: %a, removing from in_flight" Domain_name.pp name Dns_trie.pp_e e) ;
                in_flight := Domain_name.Set.remove name !in_flight ;
                Lwt.return_unit
              | Ok (ttl, tlsas) ->
                let nsupdate =
                  let add =
                    let tlsa = { Dns_packet.tlsa_cert_usage = Dns_enum.Domain_issued_certificate ;
                                 tlsa_selector = Dns_enum.Tlsa_full_certificate ;
                                 tlsa_matching_type = Dns_enum.Tlsa_no_hash ;
                                 tlsa_data = certificate }
                    in
                    Dns_packet.Add ({ Dns_packet.name ; ttl ; rdata = Dns_packet.TLSA tlsa })
                  and remove =
                    Dns_map.TlsaSet.fold
                      (fun tlsa acc ->
                         if interesting_cert tlsa then
                           Dns_packet.Remove_single (name, TLSA tlsa) :: acc
                         else
                           acc)
                      tlsas []
                  in
                  let zone = { Dns_packet.q_name = zone ; q_type = Dns_enum.SOA }
                  and update = remove @ [ add ]
                  in
                  { Dns_packet.zone ; prereq = [] ; update ; addition = []}
                and header =
                  let id = Randomconv.int16 R.generate in
                  { Dns_packet.id ; query = true ; operation = Dns_enum.Update ;
                    authoritative = false ; truncation = false ; recursion_desired = false ;
                    recursion_available = false ; authentic_data = false ; checking_disabled = false ;
                    rcode = Dns_enum.NoError }
                in
                match Dns_tsig.encode_and_sign ~proto:`Tcp header (`Update nsupdate) now dnskey key_name with
                | Error msg ->
                  in_flight := Domain_name.Set.remove name !in_flight ;
                  Logs.err (fun m -> m "Error while encoding and signing %s" msg) ;
                  Lwt.return_unit
                | Ok (data, mac) ->
                  send_dns data >>= function
                  | Error e ->
                    (* TODO: should retry DNS send *)
                    in_flight := Domain_name.Set.remove name !in_flight ;
                    Logs.err (fun m -> m "error %s while sending nsupdate" e) ;
                    Lwt.return_unit
                  | Ok () ->
                    recv_dns () >|= function
                    | Error e ->
                      (* TODO: should retry DNS send *)
                      in_flight := Domain_name.Set.remove name !in_flight ;
                      Logs.err (fun m -> m "error %s while reading DNS" e)
                    | Ok data ->
                      in_flight := Domain_name.Set.remove name !in_flight ;
                      match Dns_tsig.decode_and_verify now dnskey key_name ~mac data with
                      | Error e ->
                        Logs.err (fun m -> m "error %s while decoding nsupdate answer" e)
                      | Ok ((header, _, _, _), _) ->
                        if header.Dns_packet.rcode = Dns_enum.NoError then
                          ()
                        else
                          (* TODO: if badtime, adjust our time (to the other time) and resend ;) *)
                          Logs.err (fun m -> m "expected noerror, got %a" Dns_enum.pp_rcode header.Dns_packet.rcode))
      end
    in

    let account_key = gen_rsa (Key_gen.account_key_seed ()) in
    Conduit_mirage.with_tls ctx >>= fun ctx ->
    let ctx = Cohttp_mirage.Client.ctx res ctx in
    let directory =
      if Key_gen.production () then
        Letsencrypt.letsencrypt_url
      else
        Letsencrypt.letsencrypt_staging_url
    in
    Acme.initialise ~ctx ~directory account_key >>= function
    | Error e -> Logs.err (fun m -> m "error %s" e) ; Lwt.return_unit
    | Ok le ->
      Logs.info (fun m -> m "initialised lets encrypt") ;
      let on_update t =
        List.iter (fun zone ->
            match Domain_name.Map.find zone update_keys with
            | None -> Logs.err (fun m -> m "can't deal with %a, no update key" Domain_name.pp zone)
            | Some key ->
              let react_change name (_, tlsas) () =
                match
                  Dns_map.TlsaSet.filter interesting_csr tlsas,
                  Dns_map.TlsaSet.filter interesting_cert tlsas
                with
                | x, _ when Dns_map.TlsaSet.is_empty x ->
                  Logs.info (fun m -> m "no private selector")
                | csrs, certs ->
                  if Dns_map.TlsaSet.cardinal csrs = 1 then
                    let csr = Dns_map.TlsaSet.choose csrs in
                    let doit =
                      if Dns_map.TlsaSet.cardinal certs = 1 then
                        let cert = Dns_map.TlsaSet.choose certs in
                        if valid_and_matches_csr pclock csr cert then begin
                          Logs.info (fun m -> m "cert exists for csr, doing nothing");
                          false
                        end else begin
                          Logs.info (fun m -> m "certificate not valid or doesn't match csr, requesting");
                          true
                        end
                      else if Dns_map.TlsaSet.is_empty certs then begin
                        Logs.info (fun m -> m "no certificate found, requesting") ;
                        true
                      end else begin
                        Logs.err (fun m -> m "not prepared for this task") ;
                        false
                      end
                    in
                    if doit then
                      match X509.Encoding.parse_signing_request csr.Dns_packet.tlsa_data with
                      | None -> Logs.err (fun m -> m "couldn't parse signing request")
                      | Some csr ->
                        match List.find (function `CN _ -> true | _ -> false) (X509.CA.info csr).X509.CA.subject with
                        | exception Not_found -> Logs.err (fun m -> m "cannot find name of signing request")
                        | `CN nam ->
                          begin match Domain_name.of_string nam with
                            | Error (`Msg msg) -> Logs.err (fun m -> m "error %s while creating domain name of %s" msg nam)
                            | Ok dns_name ->
                              if not (Domain_name.equal dns_name name) then
                                Logs.err (fun m -> m "csr cn %a doesn't match dns %a" Domain_name.pp dns_name Domain_name.pp name)
                              else
                                request_certificate t le ctx zone dns_name key csr
                          end
                        | _ -> Logs.err (fun m -> m "cannot find common name of signing request")
              in
              match Dns_trie.folde zone Dns_map.Tlsa (UDns_server.Secondary.data t) react_change () with
              | Ok () -> ()
              | Error e -> Logs.warn (fun m -> m "error %a while folding" Dns_trie.pp_e e))
          (UDns_server.Secondary.zones t) ;
        Lwt.return_unit
      in
      Dns_server.secondary ~on_update stack pclock mclock dns_secondary ;
      S.listen stack
end
