open Mirage

let mirage-qubes-pin = "git+https://github.com/yomimono/mirage-qubes.git#expose-trigger-params"

let main =
  let packages = [
    package "mirage-qubes";
  ] in
  foreign
    ~packages
    "Unikernel.Main" (qubesdb @-> time @-> job)

let () =
  register "ask-update-firewall" ~argv:no_argv [
    main $ default_qubesdb $ default_time
  ]
