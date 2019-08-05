open Lwt.Infix

open Mirage_types_lwt

let src = Logs.Src.create "resolver_no_stack" ~doc:"Resolver no stack"
module Log = (val Logs.src_log src : Logs.LOG)

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (TIME : TIME) (N : NETWORK) (DB : Qubes.S.DB) = struct
  module E = Ethernet.Make(N)
  module A = Arp.Make(E)(TIME)
  module I = Qubesdb_ipv4.Make(DB)(R)(M)(E)(A)
  module U = Udp.Make(I)(R)
  module T = Tcp.Flow.Make(I)(TIME)(M)(R)

  let dns_src_ports : int list ref = ref []
  module NameMvar = Map.Make(String)

  let start _r _pclock mclock _t net db _nc =
    E.connect net >>= fun ethernet ->
    A.connect ethernet >>= fun arp ->
    I.connect db mclock ethernet arp >>= fun ipv4 ->
    U.connect ipv4 >>= fun udp ->
    T.connect ipv4 mclock >>= fun tcp ->

    let now = M.elapsed_ns mclock in
    let server =
      Dns_server.Primary.create ~rng:R.generate Dns_resolver_root.reserved in
    let resolver = ref @@ Dns_resolver.create ~mode:(`Recursive) now R.generate server in

    let name_mvar = ref NameMvar.empty in
    name_mvar := NameMvar.add "robur.io" (Lwt_mvar.create_empty ()) !name_mvar;

    let is_dns src_port dst_port =
      List.mem dst_port !dns_src_ports && src_port = 53 in

    let rec free_port () =
      let port = Cstruct.BE.get_uint16 (R.generate 2) 0 in
      if List.mem port !dns_src_ports
      then free_port ()
      else port
    in

    let send_dns_response src_port (_, dst, dst_port, buf) =
      dns_src_ports := src_port :: !dns_src_ports ;
      U.write ~src_port ~dst ~dst_port udp buf >>= fun _res ->
      Lwt.return_unit
    in

    let send_dns_query src_port (_, dst, buf) =
      dns_src_ports := src_port :: !dns_src_ports ;
      U.write ~src_port ~dst ~dst_port:53 udp buf >>= fun _res ->
      Lwt.return_unit
    in

    let handle_dns sender src_port buf =
      let p_now = Ptime.v (P.now_d_ps ()) in
      let ts = M.elapsed_ns () in
      let query_or_reply = true in
      let proto = `Udp in
      let post_request_resolver, answers, upstream_queries =
        Dns_resolver.handle_buf !resolver p_now ts query_or_reply proto sender src_port buf in
      resolver := post_request_resolver;
      Dns_resolver.stats !resolver;
      Log.info (fun f -> f "sending %d upstream queries" @@ List.length upstream_queries);
      Lwt_list.iter_p (send_dns_query @@ free_port ()) upstream_queries >>= fun () ->
      Log.info (fun f -> f "sitting on %d answers" (List.length answers));
      Lwt_mvar.put (NameMvar.find "robur.io" !name_mvar) answers >>= fun () ->
      dns_src_ports := List.filter (fun f -> f <> src_port) !dns_src_ports;
      Lwt.return_unit in

    let udp_listener = (fun ~src ~dst:_ ~src_port dst_port buf ->
        if is_dns src_port dst_port
        then handle_dns src src_port buf
        else begin
          Log.debug (fun f -> f "non-dns packet received; dropping it");
          Lwt.return_unit end)
    in

    let listeners = (fun ~dst_port -> Some (udp_listener dst_port)) in

    let udp_arg : U.ipinput = U.input ~listeners udp in

    (* ask resolver about a name, send a request, we get the reply, but we have control over the network *)
    (* at least one other service runs on it - another listener, not network but udp service *)
    (* you=firewall can't take all packets and send them to dns *)

    Lwt.async (fun () ->
    N.listen net ~header_size:Ethernet_wire.sizeof_ethernet
                  (E.input ~arpv4:(A.input arp)
                     ~ipv4:(I.input
                              ~udp:udp_arg
                              ~tcp:(fun ~src:_ ~dst:_ _contents -> Lwt.return_unit)
                              ~default:(fun ~proto:_ ~src:_ ~dst:_ _ ->
                                  (* TODO: handle ICMP destination unreachable messages here,
                                              possibly with some detailed help text? *)
                                  Lwt.return_unit)
                              ipv4
                           )
                     ~ipv6:(fun _ -> Lwt.return_unit)
                     ethernet
                  ) >>= fun _ -> Lwt.return_unit

    );

    let query_cstruct, _ = Dns_client.make_query `Udp (Domain_name.of_string_exn "robur.io") Dns.Rr_map.A in

    let send_test_queries src_port =
      let p_now = Ptime.v (P.now_d_ps ()) in
      let ts = M.elapsed_ns () in
      let query_or_reply = true in
      let proto = `Udp in
      let sender = List.hd @@ I.get_ip ipv4 in

      let new_resolver, answers, upstream_queries = Dns_resolver.handle_buf !resolver p_now ts query_or_reply proto sender src_port query_cstruct in
      resolver := new_resolver;
      Dns_resolver.stats !resolver;
      Log.info (fun f -> f "sending %d further queries" (List.length upstream_queries));
      Lwt_list.iter_p (send_dns_query src_port) upstream_queries >>= fun () ->
      Log.info (fun f -> f "%d answers known before sending any queries" (List.length answers));
      Lwt.return_unit
    in

    let rec try_queries ~retries =
      if retries <= 0 then Lwt.return_unit
      else begin
        let src_port = free_port () in
        send_test_queries src_port >>= fun () ->
        Log.info (fun f -> f "waiting 1s...");
        (* wait for mvar *)
        Lwt_mvar.take (NameMvar.find "robur.io" !name_mvar) >>= fun _hello ->
        Dns_resolver.stats !resolver;
        try_queries ~retries:(retries - 1)
      end
    in
    try_queries ~retries:2 >>= fun () ->
    Log.info (fun f -> f "all done. Resolver status: ");
    Dns_resolver.stats !resolver;
    Lwt.return_unit

end
