let statusbar = ref (GMisc.statusbar ())
let statusbar_context = ref (!statusbar#new_context "main")
let last_timeout_id = ref (Glib.Timeout.add ~ms:9999 ~callback:(fun () -> true))

let create () =
  !statusbar#set_has_resize_grip true;
  Skel.statusbar_hbox#add !statusbar#coerce;
  Glib.Timeout.remove !last_timeout_id;
  ignore (!statusbar_context#push "Welcome to Olympe 1.2")

let cleanStatusbar () =
  Glib.Timeout.remove !last_timeout_id;
  !statusbar_context#pop ();
  true

let setInfo ?(timeout=true) text =
  !statusbar_context#pop ();
  ignore (!statusbar_context#push text);
  if timeout then
    last_timeout_id := Glib.Timeout.add ~ms:5000 ~callback:cleanStatusbar
