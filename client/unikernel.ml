(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

module Main (S : Mirage_stack_lwt.V4) = struct

  module DNS = Udns_mirage_client.Make(S)

  let start s _ =
    DNS.gethostbyname (S.tcpv4 s) (Domain_name.of_string_exn "mirage.io") >|= function
    | Ok ip -> Logs.app (fun m -> m "mirage.io is at %a" Ipaddr.V4.pp ip)
    | Error (`Msg e) -> Logs.err (fun m -> m "%s while gethostbyname" e)
end
