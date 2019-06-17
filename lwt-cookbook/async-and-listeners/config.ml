open Mirage

let main = foreign "Unikernel.Main" (time @-> job)

let () =
  register "cookbook" [main $ default_time]
