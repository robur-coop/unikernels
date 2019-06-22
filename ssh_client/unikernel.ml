(* (c) 2017-2019 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

module Main (M : Mirage_clock.MCLOCK) (S : Mirage_stack_lwt.V4) = struct
  module F = S.TCPV4
  module AWA = Awa_mirage.Make(F)(M)

  let start _ s _ =
    let host = Ipaddr.V4.of_string_exn (Key_gen.host ())
    and user = Key_gen.user ()
    and authenticator =
      match Awa.Keys.authenticator_of_string (Key_gen.authenticator ()) with
      | Ok k -> k
      | Error e -> invalid_arg ("hostkey " ^ e)
    and key = Awa.Keys.of_seed (Key_gen.seed ())
    and req = Awa.Ssh.Exec (Key_gen.command ())
    in
    S.TCPV4.create_connection (S.tcpv4 s) (host, 22) >>= function
    | Error e ->
      Logs.err (fun m -> m "error %a while connection to %a"
                   S.TCPV4.pp_error e Ipaddr.V4.pp host);
      Lwt.return_unit
    | Ok flow ->
      AWA.client_of_flow ~authenticator ~user key req flow >>= function
      | Error e ->
        Logs.err (fun m -> m "error %a in client_of_flow"
                     AWA.pp_error e);
        Lwt.return_unit
      | Ok flow ->
        let rec read () =
          AWA.read flow >>= function
          | Ok `Eof -> Logs.warn (fun m -> m "received eof"); Lwt.return_unit
          | Ok `Data d -> Logs.app (fun m -> m "%s" (Cstruct.to_string d)) ; read ()
          | Error e -> Logs.err (fun m -> m "error %a while reading" AWA.pp_error e) ; Lwt.return_unit
        in
        read ()
end
