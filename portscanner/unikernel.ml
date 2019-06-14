open Mirage_types_lwt
open Lwt.Infix

let src = Logs.Src.create "portscanner" ~doc:"MirageOS port scanner example"
module Log = (val Logs.src_log src : Logs.LOG)

module Main (C: CONSOLE) (N: NETWORK) (Clock : MCLOCK) (Time: TIME) (R : RANDOM) = struct

  module E = Ethernet.Make(N)
  module A = Arp.Make(E)(Time)
  module I = Static_ipv4.Make(R)(Clock)(E)(A)
  module U = Udp.Make(I)(R)
  module T = Tcp.Flow.Make(I)(Time)(Clock)(R)

  let start c net clock _time _r =
    E.connect net >>= fun e ->
    A.connect e >>= fun a ->
    I.connect ~ip:(Ipaddr.V4.of_string_exn "10.137.0.19")
      ~network:Ipaddr.V4.(Prefix.make 24 @@ of_string_exn "10.137.0.0")
      ~gateway:(Some (Ipaddr.V4.of_string_exn "10.137.0.25"))
      clock e a >>= fun i ->
    U.connect i >>= fun udp ->
    T.connect i clock >>= fun tcp ->

    let tcp_listeners _dst_port : T.listener option =
      Some {process = (fun flow -> Lwt.return_unit);
            keepalive = None;}
    in
    let udp_listeners ~dst_port =
      Some (fun ~src:_ ~dst:_ ~src_port:_ _ -> Lwt.return_unit)
    in

    N.listen ~header_size:Ethernet_wire.sizeof_ethernet net (
      E.input
        ~arpv4:(A.input a)
        ~ipv4:(
          I.input
            ~tcp:(T.input tcp ~listeners:tcp_listeners)
            ~udp:(U.input ~listeners:udp_listeners udp)
            ~default:(fun ~proto:_ ~src:_ ~dst:_ _ -> Lwt.return_unit)
            i
        )
        ~ipv6:(fun _b -> Log.info (fun f -> f "ipv6");
                                  Lwt.return_unit) e
    )
end
