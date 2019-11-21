(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Mirage_stack.V4) = struct
  module D = Dns_resolver_mirage.Make(R)(P)(M)(T)(S)

  let start _r _pclock _mclock _ s _ =
    let now = M.elapsed_ns () in
    let server =
      Dns_server.Primary.create ~rng:R.generate Dns_resolver_root.reserved
    in
    let p = Dns_resolver.create now R.generate server in
    D.resolver ~timer:1000 ~root:true s p ;
    S.listen s
end
