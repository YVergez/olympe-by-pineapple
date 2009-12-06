let appercu1 = ref (GMisc.image ())
and appercu2 = ref (GMisc.image ())
and appercu3 = ref (GMisc.image ())
and sidebar1 = ref (GPack.vbox ())
and sidebar2 = ref (GPack.vbox ())
and sidebar3 = ref (GPack.vbox ())
and sidebar1_button = ref (GButton.button ())
and sidebar2_button = ref (GButton.button ())

(* Callback for state 1 -> 2 PRE-TREATMENT *)
let edgeImage () =
  Statusbar.setInfo "Computing edges... Please wait." ~timeout:false;
  Mainview.setMainInfoText "Computing edges... Please wait.";
  Mainview.setMainInfoImg "resources/system-run.svg";
  Mainview.showMainInfoView ();
  ignore (Glib.Main.iteration false);
  (* Iterate the main loop to display changes on the window *)
  (Skel.getWindow ())#misc#set_sensitive false;
  ignore (Glib.Main.iteration false);
  let (_,_,colors) =
    Picture_processing.process_img !Skel.map_file !Skel.edged_file
  in
  let rec add_alt = function
      [] -> []
    | (r,g,b)::t -> (r,g,b,0)::(add_alt t)
  in
    Skel.colors_alt := add_alt colors;
    Mainview.setMainMapEdgedImg !Skel.edged_file;
    Statusbar.setInfo "Computing edges... Done.";
    (Skel.getWindow ())#misc#set_sensitive true;
    Statebar.moveToState 1 ();
    Skel.showDialogAltitudes ()

(* Callback for state 2 -> 3 SAMPLING *)
let create3dModel () =
  Statusbar.setInfo "Creating 3D model... Please wait." ~timeout:false;
  Mainview.setMainInfoText "Creating 3D model... Please wait.";
  Mainview.setMainInfoImg "resources/system-run.svg";
  Mainview.new3DViewArea ();
  Mainview.showMainInfoView ();
  ignore (Glib.Main.iteration false);
  (Skel.getWindow ())#misc#set_sensitive false;
  ignore (Glib.Main.iteration false);
  Sampling.openbmp !Skel.map_file !Skel.obj_file !Skel.colors_alt !Skel.step;
  Skel.display_ids := Display3D.draw_map "-f"
    !Skel.obj_file
    ~gui:true
    ~win:(Skel.getWindow ())
    ~box:(Mainview.get3DViewArea ())
    ~allow:Skel.allow_inputs;
  Statusbar.setInfo "Displaying 3D model." ~timeout:true;
  (Skel.getWindow ())#misc#set_sensitive true;
  Statebar.moveToState 2 ()

(* Create the appercu area and its image *)
let createAppercu n () =
  let appercu_area = GBin.frame
    ~label:"Preview"
    ~label_xalign:0.05
    ~width:200
    ~height:210 ()
  in
  let appercu_img = GMisc.image
    ~show:true
    ~packing:appercu_area#add ()
  in
    (match n with
	1 -> appercu1 := appercu_img
      | 2 -> appercu2 := appercu_img
      | 3 -> appercu3 := appercu_img
      | _ -> ());
    appercu_area

(* Create sidebar #1 *)
let createSidebar1 () =
  !sidebar1#pack (createAppercu 1 ())#coerce;
  let computeButt = GButton.button
    ~label:"Compute edges"
    ~packing:(!sidebar1#pack ~expand:false) () in
    computeButt#misc#set_sensitive false;
    ignore (computeButt#connect#clicked
      ~callback:edgeImage);
    sidebar1_button := computeButt;
    Skel.sidebar_vbox#add !sidebar1#coerce

(* Create sidebar #2 *)
let updateStep adj () =
  Skel.step := int_of_float adj#value;
  Mainview.refreshStepOnEdgedImg ()

   (* --- dialog to select grid color --- *)
let showColorSelector () =
  let win = GWindow.color_selection_dialog
    ~title:"Change grid color"
    ~parent:(Skel.getWindow ())
    ~destroy_with_parent:true
    ~modal:true
    ~position:`CENTER_ON_PARENT ()
  in

    win#colorsel#set_color !Mainview.grid_color;

    match win#run () with
 	`OK ->
 	  Mainview.grid_color :=
	    (Gdk.Color.alloc
	       ~colormap:(Gdk.Color.get_system_colormap ())
	       (`RGB((Gdk.Color.red win#colorsel#color),
		     (Gdk.Color.green win#colorsel#color),
		     (Gdk.Color.blue win#colorsel#color))));
	  Mainview.refreshStepOnEdgedImg ();
	  win#destroy ()
      | _ -> win#destroy ()

let createSidebar2 () =
  !sidebar2#misc#hide ();
  !sidebar2#pack (createAppercu 2 ())#coerce;

  let step_slider = GData.adjustment
    ~value:30.
    ~lower:10.
    ~upper:60.
    ~step_incr:10.
    ~page_incr:20.
    ~page_size:10. ()
  in

  let _ = GMisc.label
    ~text:"Step"
    ~xalign:0.0
    ~xpad:0
    ~packing:(!sidebar2#pack ~expand:false) ()
  in

  let slider = GRange.scale
    `HORIZONTAL
    ~adjustment:step_slider
    ~digits:0
    ~draw_value:true
    ~value_pos:`LEFT
    ~packing:(!sidebar2#pack ~expand:false) () in

  let colorButton = GButton.button
    ~label:"Change grid color"
    ~packing:(!sidebar2#pack ~expand:false) () in

  let check_edges = GButton.check_button
    ~label:"Use relief borders"
    ~use_mnemonic:true
    ~active:true
    ~draw_indicator:true
    ~packing:(!sidebar2#pack ~expand:false) () in

  let computeButt = GButton.button
    ~label:"Compute 3D model"
    ~packing:(!sidebar2#pack ~expand:false) () in
    computeButt#misc#set_sensitive true;
    ignore (computeButt#connect#clicked
	      ~callback:create3dModel);

    ignore (slider#connect#value_changed
	      ~callback:(updateStep slider#adjustment));
    ignore (colorButton#connect#clicked
	      ~callback:showColorSelector);
    ignore (check_edges#connect#toggled
	      ~callback:(fun () -> Skel.use_edges := not !Skel.use_edges));

    sidebar2_button := computeButt;
    Skel.sidebar_vbox#pack ~expand:false !sidebar2#coerce

(* Create sidebar #3 *)
let createSidebar3 () =
  !sidebar3#misc#hide ();
  !sidebar3#pack (createAppercu 3 ())#coerce;
  Skel.sidebar_vbox#pack ~expand:false !sidebar3#coerce

(* Create all sidebars *)
let create () =
  createSidebar1 ();
  createSidebar2 ();
  createSidebar3 ()

let changeAppercuImg file =
  !appercu1#set_file file;
  !appercu2#set_file file;
  !appercu3#set_file file

let changeAppercuImgByPixbuf pixbuf =
  !appercu1#set_pixbuf pixbuf;
  !appercu2#set_pixbuf pixbuf;
  !appercu3#set_pixbuf pixbuf

let setMainButtonSensitive bool = function
    0 -> !sidebar1_button#misc#set_sensitive bool
  | 1 -> !sidebar2_button#misc#set_sensitive bool
  | _ -> ()

let get = function
    0 -> !sidebar1
  | 1 -> !sidebar2
  | 2 -> !sidebar3
  | n -> failwith ("Invalid call to Sidebar.get : " ^ (string_of_int n) ^ "th element do not exist.")
