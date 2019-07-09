open Lwt.Infix

open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (N : NETWORK) (DB : Qubes.S.DB) = struct
  module E = Ethernet.Make(N)
  module A = Arp.Make(E)(T)
  module I = Qubesdb_ipv4.Make(DB)(R)(M)(E)(A)
  module U = Udp.Make(I)(R)
  module T = Tcp.Flow.Make(I)(T)(M)(R)

  let start _r _pclock mclock _t net db _nc =
    E.connect net >>= fun ethernet ->
    A.connect ethernet >>= fun arp ->
    I.connect db mclock ethernet arp >>= fun ipv4 ->
    U.connect ipv4 >>= fun udp ->
    T.connect ipv4 mclock >>= fun tcp ->

    let now = M.elapsed_ns mclock in
    let server =
      Dns_server.Primary.create ~rng:R.generate Dns_resolver_root.reserved in
    let _resolver = Dns_resolver.create ~mode:(`Recursive) now R.generate server in

    (* TODO add dns listener capacity - dispatch needs to happen in udp_arg *)
    let udp_listener : U.callback = (fun ~src ~dst:_ ~src_port buf -> Lwt.return_unit) in
    let udp_arg : U.ipinput = U.input ~listeners:(fun ~dst_port:_ -> Some udp_listener) udp in

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

    Lwt.return_unit

end
