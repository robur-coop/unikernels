(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) = struct
  module D = Udns_mirage_server.Make(P)(M)(T)(S)

  let start _rng pclock mclock _ s _ =
    let keys = List.fold_left (fun acc str ->
        match Udns_packet.name_dnskey_of_string str with
        | Error (`Msg msg) -> Logs.err (fun m -> m "key parse error: %s" msg) ; acc
        | Ok (name, key) -> (name, key) :: acc)
        [] (Key_gen.keys ())
    in
    let t =
      Udns_server.Secondary.create ~a:[ Udns_server.Authentication.tsig_auth ]
        ~tsig_verify:Udns_tsig.verify ~tsig_sign:Udns_tsig.sign
        ~rng:R.generate keys
    in
    D.secondary s t ;
    S.listen s
end
