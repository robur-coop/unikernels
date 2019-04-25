(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

module Main (S : Mirage_stack_lwt.V4) = struct

  module DNS = Udns_mirage_client.Make(S)

  let start s _ =
    let t = DNS.create s in
    let host = Domain_name.of_string_exn (Key_gen.hostname ()) in
    DNS.gethostbyname t host >|= function
    | Ok ip -> Logs.app (fun m -> m "%a is at %a" Domain_name.pp host Ipaddr.V4.pp ip)
    | Error (`Msg e) -> Logs.err (fun m -> m "%s while gethostbyname" e)
end
