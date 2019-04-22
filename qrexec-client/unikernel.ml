open Lwt.Infix

module Main (DB : Qubes.S.DB) (Time : Mirage_time_lwt.S) = struct
  let service_name = "yomimono.updateFirewall"
  let target_domain = "dom0"

  let handler ~user cmdline flow =
    Logs.info (fun f -> f "received qrexec message: user %s, message %s" user cmdline);
    let rec print_next_line () =
      Qubes.RExec.Stdout.read_line flow >>= function
      | `Eof -> Lwt.return 0
      | `Ok input ->
        Logs.info (fun f -> f "%s" input);
        print_next_line ()
    in
    print_next_line ()

  let start _db _time =
    Qubes.RExec.connect ~domid:0 () >>= fun qrexec ->
    Qubes.RExec.request_service qrexec ~target_domain ~service_name handler >|= function
    | Error (`Msg s) -> Logs.err (fun f -> f "unknown error: %s" s)
    | Error `Permission_denied -> Logs.err (fun f -> f "Permission denied for service %s" service_name)
    | Error `Closed -> Logs.err (fun f -> f "tried to write to a closed qrexec channel")
    | Ok () -> Logs.info (fun f -> f "successfully ran qrexec request")
end
