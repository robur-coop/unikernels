(* (c) 2018 Hannes Mehnert, all rights reserved *)
open Lwt.Infix

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S: Mirage_stack.V4) = struct
  module D = Dns_certify_mirage.Make(R)(P)(T)(S)
  module TLS = Tls_mirage.Make(S.TCPV4)

  let rec handle flow =
    TLS.read flow >>= function
    | Ok `Eof ->
      Logs.info (fun f -> f "Closing connection!") ;
      TLS.close flow
    | Error e ->
      Logs.warn (fun f -> f "Error reading data from established connection: %a" TLS.pp_error e) ;
      TLS.close flow
    | Ok (`Data data) ->
      Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len data) (Cstruct.to_string data));
      TLS.write flow data >>= function
      | Ok () -> handle flow
      | Error e ->
        Logs.warn (fun m -> m "error %a while echoing" TLS.pp_write_error e) ;
        TLS.close flow

  let accept conf handle flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "new tls connection from IP %s on port %d"
                  (Ipaddr.V4.to_string dst) dst_port);
    TLS.server_of_flow conf flow >>= function
    | Ok tls ->
      (match TLS.epoch tls with
       | Ok e ->
         Logs.info (fun m -> m "established TLS %a %a,%a,extended_ms=%b"
                       Sexplib.Sexp.pp_hum (Tls.Core.sexp_of_tls_version e.Tls.Core.protocol_version)
                       Sexplib.Sexp.pp_hum (Tls.Ciphersuite.sexp_of_ciphersuite e.Tls.Core.ciphersuite)
                       Fmt.(option ~none:(unit "no SNI") string) e.Tls.Core.own_name
                       e.Tls.Core.extended_ms)
       | Error () ->
         Logs.warn (fun m -> m "error while retrieving TLS epoch")) ;
      handle tls
    | Error e ->
      Logs.err (fun m -> m "TLS handshake error %a" TLS.pp_write_error e) ;
      Lwt.return_unit

  (* TODO: move to TLS *)
  let log_certchain (chain, priv) =
    let certs =
      String.concat "\n" (List.map (fun c ->
          Cstruct.to_string (X509.Certificate.encode_pem c))
          chain)
    and key =
      Cstruct.to_string (X509.Private_key.encode_pem (`RSA priv))
    in
    Logs.app (fun m -> m "certificate chain:@.%s" certs);
    Logs.app (fun m -> m "private key:@.%s" key)

  let start _random _pclock _mclock _ stack =
    let hostname = Domain_name.(host_exn (of_string_exn (Key_gen.hostname ()))) in
    let additional_hostnames =
      List.map (fun n -> Domain_name.(host_exn (of_string_exn n)))
        (Astring.String.cuts ~empty:false ~sep:"," (Key_gen.additional ()))
    in
    D.retrieve_certificate stack ~dns_key:(Key_gen.dns_key ())
      ~hostname ~additional_hostnames ?key_seed:(Key_gen.key_seed ())
      (Key_gen.dns_server ()) (Key_gen.dns_port ()) >>= function
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error %s while retrieving certificate, giving up" msg);
      Lwt.return_unit
    | Ok own_cert ->
      (match own_cert with
      | `None -> Logs.err (fun m -> m "own_cert is none")
      | `Single chain -> log_certchain chain
      | `Multiple chains -> List.iter log_certchain chains
      | `Multiple_default (chain, chains) ->
        log_certchain chain ;
        List.iter log_certchain chains) ;
      let config = Tls.Config.server ~certificates:own_cert () in
      S.listen_tcpv4 stack ~port:(Key_gen.port ()) (accept config handle) ;
      S.listen stack
end
