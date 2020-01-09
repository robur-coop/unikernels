(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Mirage_stack.V4) = struct
  module D = Dns_server_mirage.Make(P)(M)(T)(S)

  let start _rng _pclock _mclock _ s _ =
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
