let statusbar = ref (GMisc.statusbar ())
let statusbar_context = ref (!statusbar#new_context "main")

let create () =
  !statusbar#set_has_resize_grip true;
  Skel.statusbar_hbox#add !statusbar#coerce;
  ignore (!statusbar_context#push "Welcome to Olympe 1.2")

let setInfo text =
  ignore (!statusbar_context#push text)
