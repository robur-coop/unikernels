open Lwt.Infix

let src = Logs.Src.create "unikernel" ~doc:"Main unikernel code"
module Log = (val Logs.src_log src : Logs.LOG)

module Main(Time : Mirage_time_lwt.S) = struct

  let do_it () =
    let counter = ref 1 in
    let can_cancel = fst @@ Lwt.task () in
    Lwt.try_bind (fun () -> (
          Lwt.async (fun () ->
              let rec forever () =
                Logs.info (fun f -> f "Hello World");
                if !counter = 3 then Lwt.cancel can_cancel
                else counter := !counter + 1;
                Time.sleep_ns 500_000_000L >>= forever
              in
              forever ()
            );
          can_cancel)
      )
      (fun _ -> Logs.info (fun f -> f "a promise we expected to never successfully complete... did????");
        Lwt.return_unit)
      (fun Lwt.Canceled ->
        Logs.info (fun f -> f "canceled!!");
        Lwt.return_unit)

  let start _time =
    do_it () >>= fun () ->
    do_it ()

end
