open Lwt.Infix

let src = Logs.Src.create "firewall_reader" ~doc:"FW reader"
module Log = (val Logs.src_log src : Logs.LOG)

module Main(Time : Mirage_time_lwt.S) = struct

  let start t =
    Qubes.DB.connect ~domid:0 () >>= fun qubesDB ->
    let bindings = Qubes.DB.bindings qubesDB in
    let rec print_and_loop bindings =
      Qubes.DB.KeyMap.iter (fun k v -> Logs.info (fun m -> m "%s %s" k v)) bindings;
      Qubes.DB.after qubesDB bindings >>= print_and_loop
    in
    print_and_loop bindings
    (* Time.sleep_ns 1_000_000L >>= fun () -> *)

end
