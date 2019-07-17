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

  let start _r _pclock mclock _t net db _nc =
    E.connect net >>= fun ethernet ->
    A.connect ethernet >>= fun arp ->
    I.connect db mclock ethernet arp >>= fun ipv4 ->
    U.connect ipv4 >>= fun udp ->
    T.connect ipv4 mclock >>= fun tcp ->

    let now = M.elapsed_ns mclock in
    let server =
      Dns_server.Primary.create ~rng:R.generate Dns_resolver_root.reserved in
    let resolver = Dns_resolver.create ~mode:(`Recursive) now R.generate server in

    (* TODO add dns listener capacity - dispatch needs to happen in udp_arg *)

    let is_dns src_port dst_port =
      (* TODO match dest port to our records *)
      src_port = 53 in

    let handle_dns sender source_port buf =
      let p_now = Ptime.v (P.now_d_ps ()) in
      let ts = M.elapsed_ns () in
      let query_or_reply = true in
      let proto = `Udp in
      let dns_handler, _, _ = Dns_resolver.handle_buf resolver p_now ts query_or_reply proto sender source_port buf in
      Dns_resolver.stats dns_handler;
      Lwt.return_unit in

    let udp_listener = (fun ~src ~dst:_ ~src_port dst_port buf ->
        Log.info (fun m -> m "Reply content %a" Cstruct.hexdump_pp buf);
        if is_dns src_port dst_port
        then handle_dns src src_port buf
        else Lwt.return_unit) in

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


    let p_now = Ptime.v (P.now_d_ps ()) in
    let ts = M.elapsed_ns () in
    let query_or_reply = true in
    let proto = `Udp in
    let sender = List.hd @@ I.get_ip ipv4 in
    let source_port = 7777 in

    let query_sender (_, dst, dst_port, buf) =
      U.write ~dst ~dst_port udp buf >>= fun _res ->
      Lwt.return_unit
    in

    (*
         outgoing_packets : (Dns.proto * Ipaddr.V4.t * int * Cstruct.t) list
     * *)
    let dns_handler, outgoing_packets, _ = Dns_resolver.handle_buf resolver p_now ts query_or_reply proto sender source_port query_cstruct in
    Dns_resolver.stats dns_handler;
    Lwt_list.iter_s query_sender outgoing_packets >>= fun () ->

    Log.info (fun m -> m "%a" Cstruct.hexdump_pp query_cstruct);
    TIME.sleep_ns 1_000_000_000L

end
