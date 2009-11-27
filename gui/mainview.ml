(*type target_entry = { target: string; flags: Gtk.Tags.target_flags list; info: int }*)

let main_map_img = ref (GMisc.image ())
and main_map_edged_img = ref (GMisc.image ())
and main_map = ref (GBin.scrolled_window
		      ~hpolicy:`AUTOMATIC
		      ~vpolicy:`AUTOMATIC ())
and main_map_edged = ref (GBin.scrolled_window
			    ~hpolicy:`AUTOMATIC
			    ~vpolicy:`AUTOMATIC ())
and main_3d_view = ref (GlGtk.area [`RGBA;`DEPTH_SIZE 1;`DOUBLEBUFFER] ())
and main_info_view = (GPack.vbox ())
and main_info_text = (GMisc.label ~justify:`CENTER ~line_wrap:true ())
and main_info_img  = (GMisc.image ~icon_size:`LARGE_TOOLBAR ())

let setMainInfoText text =
  main_info_text#set_text text

let setMainInfoImg file =
  main_info_img#set_file file

let setMainInfoImgByStock stock =
  main_info_img#set_stock stock

let init () =
  Skel.mainview_vbox#add !main_map#coerce;
  Skel.mainview_vbox#add !main_map_edged#coerce;
  Skel.mainview_vbox#add !main_3d_view#coerce;
  Skel.mainview_vbox#add main_info_view#coerce;

  main_info_view#add (GPack.hbox ())#coerce;
  main_info_view#pack ~expand:false main_info_img#coerce;
  main_info_view#pack ~expand:false main_info_text#coerce;
  main_info_view#add (GPack.hbox ())#coerce;

  setMainInfoText "1. Add an image to treat using the \"Open image\" button in the toolbar or drag and drop an image file here.";
  setMainInfoImg "resources/toolbar/insert-image.svg";

  !main_map#add_with_viewport !main_map_img#coerce;
  !main_map_edged#add_with_viewport !main_map_edged_img#coerce;
  !main_map#misc#hide ();
  !main_map_edged#misc#hide ();
  !main_3d_view#misc#hide ()

let setMainMapImg file =
  !main_map_img#set_file file

let setMainMapEdgedImg file =
  !main_map_edged_img#set_file file

let showMainMap () =
  !main_map#misc#show ();
  !main_map_edged#misc#hide ();
  !main_3d_view#misc#hide ();
  main_info_view#misc#hide ()

let showMainMapEdged () =
  !main_map#misc#hide ();
  !main_map_edged#misc#show ();
  !main_3d_view#misc#hide ();
  main_info_view#misc#hide ()

let showMain3DView () =
  !main_map#misc#hide ();
  !main_map_edged#misc#hide ();
  !main_3d_view#misc#show ();
  main_info_view#misc#hide ()

let showMainInfoView () =
  !main_map#misc#hide ();
  !main_map_edged#misc#hide ();
  !main_3d_view#misc#hide ();
  main_info_view#misc#show ()

let get3DViewArea () =
  !main_3d_view
