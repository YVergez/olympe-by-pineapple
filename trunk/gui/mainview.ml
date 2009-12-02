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
and main_info_img  = (GMisc.image ())
and edged_img_drawarea = ref (new GDraw.drawable (GDraw.pixmap ~width:10 ~height:10 ())#pixmap)

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

  setMainInfoText "Open an image to render using the \"Open image\" button in the toolbar or drag and drop an image file here.";
  setMainInfoImg "resources/toolbar/insert-image.svg";

  !main_map#add_with_viewport !main_map_img#coerce;
  !main_map_edged#add_with_viewport !main_map_edged_img#coerce;
  !main_map#misc#hide ();
  !main_map_edged#misc#hide ();
  !main_3d_view#misc#hide ()

let setMainMapImg file =
  !main_map_img#set_file file

let refreshStepOnEdgedImg () =
  let area = !edged_img_drawarea in
  let s = !Skel.step
  and (w,h) = area#size in
    for i = 1 to w do
      area#line ~x:(s * i) ~y:0 ~x:(s * i) ~y:h
    done;
    for i = 1 to h do
      area#line ~x:0 ~y:(s * i) ~x:w ~y:(s * i)
    done;
    
    print_endline (string_of_int s)

let setMainMapEdgedImg file =
  let edged_pixmap = GDraw.pixmap_from_xpm file () in
    edged_img_drawarea := new GDraw.drawable
      edged_pixmap#pixmap;
    !main_map_edged_img#set_pixmap edged_pixmap;
    refreshStepOnEdgedImg ()

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
