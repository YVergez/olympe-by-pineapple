let main_map_img = ref (GMisc.image ())
and main_map_edged_img = ref (GMisc.image ())
and main_map = ref (GBin.scrolled_window ())
and main_map_edged = ref (GBin.scrolled_window ())
and main_3d_view = ref (GlGtk.area [`RGBA;`DEPTH_SIZE 1;`DOUBLEBUFFER] ())

let init () =
  Skel.mainview_vbox#add !main_map#coerce;
  Skel.mainview_vbox#add !main_map_edged#coerce;
  Skel.mainview_vbox#add !main_3d_view#coerce;

  !main_map#add_with_viewport !main_map_img#coerce;
  !main_map_edged#add_with_viewport !main_map_edged_img#coerce;
  !main_map_edged#misc#hide ();
  !main_3d_view#misc#hide ()

let setMainMapImg file =
  !main_map_img#set_file file

let setMainMapEdgedImg file =
  !main_map_edged_img#set_file file

let showMainMap () =
  !main_map#misc#show ();
  !main_map_edged#misc#hide ();
  !main_3d_view#misc#hide ()

let showMainMapEdged () =
  !main_map#misc#hide ();
  !main_map_edged#misc#show ();
  !main_3d_view#misc#hide ()

let showMain3DView () =
  !main_map#misc#hide ();
  !main_map_edged#misc#hide ();
  !main_3d_view#misc#show ()

let get3DViewArea () =
  !main_3d_view
