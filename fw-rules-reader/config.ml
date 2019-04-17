open Mirage

let main =
  foreign
    ~packages:[ package "mirage-qubes" ]
    "Unikernel.Main" (time @-> job)

let () =
  register "fw-rules-reader" [main $ default_time]
