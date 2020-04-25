(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

module Main (R : Mirage_random.S) (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) (S : Mirage_stack.V4) = struct

  module DNS = Dns_client_mirage.Make(R)(T)(M)(S)

  let start _ _ _ s =
    let t = DNS.create s in
    let host = Domain_name.(host_exn (of_string_exn (Key_gen.hostname ()))) in
    DNS.gethostbyname t host >|= function
    | Ok ip -> Logs.app (fun m -> m "%a is at %a" Domain_name.pp host Ipaddr.V4.pp ip)
    | Error (`Msg e) -> Logs.err (fun m -> m "%s while gethostbyname" e)
end
