let statusbar = ref (GMisc.statusbar ())
let statusbar_context = ref (!statusbar#new_context "main")
let last_timeout_id = ref (Glib.Timeout.add ~ms:9999 ~callback:(fun () -> true))

let create () =
  !statusbar#set_has_resize_grip true;
  Skel.statusbar_hbox#add !statusbar#coerce;
  Glib.Timeout.remove !last_timeout_id;
  ignore (!statusbar_context#push "Welcome to Olympe 1.0")

let cleanByTimeout id () =
  Glib.Timeout.remove !last_timeout_id;
  !statusbar_context#remove id;
  true

let clean () =
  !statusbar_context#pop ()

let setInfo ?(timeout=true) text =
  clean ();
  let id = !statusbar_context#push text in
  if timeout then
    last_timeout_id := Glib.Timeout.add ~ms:5000 ~callback:(cleanByTimeout id)

let getContext () =
  statusbar_context
