(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (M : MCLOCK) (T : TIME) (S : STACKV4) = struct
  module D = Dns_mirage_resolver.Make(R)(P)(M)(T)(S)

  let start _r pclock mclock _ s _ =
    let now = M.elapsed_ns mclock in
    let server =
      Dns_server.Primary.create ~rng:R.generate Dns_resolver_root.reserved
    in
    let p = Dns_resolver.create now R.generate server in
    D.resolver ~timer:1000 ~root:true s p ;
    S.listen s
end
