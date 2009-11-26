let appercu1 = ref (GMisc.image ())
and appercu2 = ref (GMisc.image ())
and appercu3 = ref (GMisc.image ())
and sidebar1 = ref (GPack.vbox ())
and sidebar2 = ref (GPack.vbox ())
and sidebar3 = ref (GPack.vbox ())
and sidebar1_button = ref (GButton.button ())
and sidebar2_button = ref (GButton.button ())
and sidebar3_button = ref (GButton.button ())

(* Callback for state 1 -> 2 PRE-TREATMENT *)
let edgeImage () =
  Statusbar.setInfo "Computing edges... Please wait.";
  let (_,_,colors) = Picture_processing.process_img !Skel.map_file !Skel.edged_file
  in
  let rec add_alt = function
      [] -> []
    | (r,g,b)::t -> (r,g,b,0)::(add_alt t)
  in
    Skel.colors_alt := add_alt colors;
    Skel.showOnlyChild 1 Skel.toolbar_vbox;
    Skel.showOnlyChild 1 Skel.sidebar_vbox;
    Mainview.setMainMapEdgedImg !Skel.edged_file;
    Mainview.showMainMapEdged ();
    Skel.showDialogAltitudes ();
    Statusbar.setInfo "Computing edges... Done."

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
let createSidebar2 () =
  !sidebar2#misc#hide ();
  !sidebar2#pack (createAppercu 2 ())#coerce;
  let computeButt = GButton.button
    ~label:"Compute 3D model"
    ~packing:(!sidebar2#pack ~expand:false) () in
    computeButt#misc#set_sensitive true;
    ignore (computeButt#connect#clicked
      ~callback:Skel.void);
    sidebar2_button := computeButt;
    Skel.sidebar_vbox#pack ~expand:false !sidebar2#coerce

(* Create sidebar #3 *)
let createSidebar3 () =
  !sidebar3#misc#hide ();
  !sidebar3#pack (createAppercu 3 ())#coerce;
  let computeButt = GButton.button
    ~label:"3D"
    ~packing:(!sidebar3#pack ~expand:false) () in
    computeButt#misc#set_sensitive false;
    ignore (computeButt#connect#clicked
      ~callback:Skel.void);
    sidebar3_button := computeButt;
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
  | 2 -> !sidebar3_button#misc#set_sensitive bool
  | _ -> ()

let get = function
    0 -> !sidebar1
  | 1 -> !sidebar2
  | 2 -> !sidebar3
  | n -> failwith ("Invalid call to Sidebar.get : " ^ (string_of_int n) ^ "th element do not exist.")
