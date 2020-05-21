(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
module Main (C : Mirage_console.S) (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Mirage_stack.V4) (Management : Mirage_stack.V4) = struct
  module D = Dns_server_mirage.Make(P)(M)(T)(S)

  module Monitoring = Monitoring_experiments.Make(T)(Management)
  module Syslog = Logs_syslog_mirage.Udp(C)(P)(Management)

  let start c _rng _pclock _mclock _ s management =
    let hostname = Key_gen.name ()
    and syslog = Key_gen.syslog ()
    and monitor = Key_gen.monitor ()
    in
    if Ipaddr.V4.compare syslog Ipaddr.V4.unspecified = 0 then
      Logs.warn (fun m -> m "no syslog specified, dumping on stdout")
    else
      Logs.set_reporter (Syslog.create c management syslog ~hostname ());
    if Ipaddr.V4.compare monitor Ipaddr.V4.unspecified = 0 then
      Logs.warn (fun m -> m "no monitor specified, not outputting statistics")
    else
      Monitoring.create ~hostname monitor management;
    let keys = List.fold_left (fun acc str ->
        match Dns.Dnskey.name_key_of_string str with
        | Error (`Msg msg) -> Logs.err (fun m -> m "key parse error %s" msg) ; exit 64
        | Ok (name, key) -> (name, key) :: acc)
        [] (Key_gen.keys ())
    in
    let t =
      Dns_server.Secondary.create ~rng:R.generate
        ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign keys
    in
    D.secondary s t ;
    S.listen s
end
