(* All components of the gui : *)
let img_src = ref ""
and pre_img_src = ref "resources/tmp/pre_img.bmp"
and inter = ref 42
and obj_file_src = ref "resources/tmp/final_obj.obj"
and prog_state = ref 0

and window = ref (GWindow.window ())
and loading_window = ref (GWindow.dialog ())
and main_vbox = ref (GPack.vbox ())
and menubar = ref (GMenu.menu_bar ())
and toolbars = ref (Array.make 3 (GButton.toolbar ()))
and program_state_hbox = ref (GPack.hbox ())
and state_buttons = ref (Array.make 3 (GButton.button ()))
and main_view_hbox = ref (GPack.hbox ())
and sidebar_vbox = ref (GPack.vbox ())
and sidebar_button = ref (GButton.button ())
and imgPreview = ref (GMisc.image ())
and statusbar = ref ((GMisc.statusbar ())#new_context ~name:"program_state")
and about_win = ref (GWindow.about_dialog ())
and open_win = ref (GWindow.file_chooser_dialog ~action:`OPEN ~title:"NULL" ())
and main_img = ref (GMisc.image ())
and gl_area = ref (GlGtk.area [`RGBA;`DEPTH_SIZE 1;`DOUBLEBUFFER] ())

(*An useless fonction*)
let void () = ()

(*Terminate application*)
let destroy () =
  GMain.Main.quit ()

(* Change button apparence to running *)
let create_loading_window ?(text="Loading...\nPlease wait.") () =
  let win = GWindow.dialog
    ~no_separator:true
    ~parent:!window
    ~destroy_with_parent:true
    ~title:"Loading..."
    ~deletable:false
    ~modal:true
    ~position:`CENTER_ON_PARENT
    ~resizable:false
    ~width:190
    ~height:100
    ~show:false () in
    GMisc.label
      ~text:text
      ~packing:win#vbox#add ();
    let img = GMisc.image
      ~file:"resources/gnome-foot.gif"
      ~packing:win#vbox#add () in
      loading_window := win;
      win#misc#show ()

(* Create a miniature file from filename *)
let miniature ~width ~height ~filename () =
  GdkPixbuf.from_file_at_size
    filename
    width
    height

(* --- CHANGING GUI APPEARANCE --- *)
let rec update_gui_status state  () =
  match state with
      0 -> update_gui_status 1  ();
    | 1 ->
	(Array.of_list !menubar#children).(1)#misc#set_sensitive false;
	!toolbars.(0)#misc#show ();
	!toolbars.(1)#misc#hide ();
	!toolbars.(2)#misc#hide ();
    | 2 ->
	(Array.of_list !menubar#children).(1)#misc#set_sensitive false;
	!toolbars.(0)#misc#hide ();
	!toolbars.(1)#misc#show ();
	!toolbars.(2)#misc#hide ();
	!main_img#set_file !pre_img_src;
    | 3 ->
	(Array.of_list !menubar#children).(1)#misc#set_sensitive true;
	!toolbars.(0)#misc#hide ();
	!toolbars.(1)#misc#hide ();
	!toolbars.(2)#misc#show ();
    | _ -> ()

(*Ask the user if he really want to terminate the application*)
let ask_destroy ev =
  let ask_dialog = GWindow.message_dialog
    ~modal:true
    ~message:"Do you really want to exit the application ?"
    ~use_markup:true
    ~message_type:`INFO
    ~buttons:GWindow.Buttons.yes_no
    ~destroy_with_parent:true
    ~title:"Quit ?"
    ~position:`CENTER_ON_PARENT
    ~resizable:false () in
    match ask_dialog#run () with
      | `YES ->
	  destroy ();
	  false
      | _    ->
	  ask_dialog#destroy ();
	  true

(* --- ABOUT WINDOW --- *)
let licence2string =
  let file = open_in "resources/gpl-3.0.txt" in
  let rec cat_in result =
    try
      cat_in (result ^ (input_line file) ^ "\n")
    with End_of_file -> close_in file; result
  in
    cat_in ""

let create_about_win () =
  let win = GWindow.about_dialog
    ~authors:
    ["Guillaume Algis [algis_g@epita.fr]";
     "Perrine Brunel [brunel_a@epita.fr]";
     "Hugo Damme [damme_h@epita.fr]";
     "Yohann Vergez [vergez_y@epita.fr]"]
    ~copyright:"Copyright 2009 Pineapple"
    ~license:licence2string
    ~logo:((GMisc.image ~file:"resources/olympe-logo.png" ())#pixbuf)
    ~name:"Olympe"
    ~title:"Olympe"
    ~version:"0.9"
    ~website:"http://olympe-pineapple.fr.nf/"
    ~website_label:"Olympe developpers' blog"
    ~parent:!window
    ~destroy_with_parent:true
    ~modal:false
    ~position:`CENTER_ON_PARENT
    ~resizable:false
    ~show:false () in
    win

let init_about_win () =
  let hide_about ev =
    !about_win#misc#hide ()
  and event_hide_about ev =
    !about_win#misc#hide ();
    true
  in
    !about_win#connect#response ~callback:hide_about;
    !about_win#event#connect#delete
      ~callback:event_hide_about;
    ()

(* PRE-TREATMENT *)
(* Edge the current file *)
let edge_image () =
  Picture_processing.process_img !img_src !pre_img_src;
  update_gui_status 2 ();
  prog_state := 2

(* Check if the selectioned file is a .bmp *)
let openwin_verify_file () =
  (match !open_win#get_filenames with
      [] -> ()
    | filename::_ ->
	let ext =
	  try
	    String.sub filename (String.rindex filename '.') 4
	  with Not_found -> "DIR"
	in
	  if ext = ".bmp" then
	    begin
	      prog_state := 1;
	      img_src := filename;
	      !main_img#set_file filename;
	      !main_img#misc#show ();
	      !imgPreview#set_pixbuf (miniature
		~filename:filename
		~width:180
		~height:180 ());
	      !imgPreview#misc#show ();
	      !sidebar_button#misc#set_sensitive true;
	    end);
  !open_win#destroy ()

(* Create open file window *)
let open_fileWin () =
  let open_window = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~parent:!window
    ~destroy_with_parent:true
    ~title:"Choose an image file"
    ~icon:(GMisc.image ~file:"resources/toolbar/insert-image.svg" ())#pixbuf
    ~modal:true
    ~position:`CENTER_ON_PARENT
    ~resizable:true
    ~show:false () in
    open_window#add_select_button_stock `OK `VALID;

    (* Adding filter on .bmp *)
    let filter = GFile.filter
      ~name:"BMP files"
      ~patterns:["*.bmp"] ()
    in
      open_window#add_filter filter;
      open_win := open_window;

      (match open_window#run () with
	   `VALID -> openwin_verify_file ()
	 | `DELETE_EVENT -> open_window#destroy ()
	 | _ -> ());
      flush stdout


(* MAIN WINDOW *)
(*Create the main window*)
let create_main_window () =
  let window = GWindow.window
    ~position:`CENTER
    ~width:800
    ~height:600
    ~resizable:true
    ~allow_grow:true
    ~allow_shrink:true
    ~icon:((GMisc.image ~file:"resources/icon.png" ())#pixbuf)
    ~title:"Olympe pre-beta GUI" () in
    window#event#connect#delete ~callback:ask_destroy;
    window#event#connect#destroy ~callback:ask_destroy;
    window#connect#destroy ~callback:destroy;
    window

(* --- MENU BAR --- *)
(*Add a item from stocks to menu*)
let add_stock_item menu ~stock ~callback () =
  let item = GMenu.image_menu_item
    ~stock
    ~packing:menu#append () in
    item#connect#activate ~callback;
    item

(*Add a separator item to menu*)
let add_separator menu () =
  GMenu.separator_item
    ~packing:menu#append ()

(*Add a submenu to menu*)
let add_submenu menu ~label ~filename ~subitems () =
  let icon = GMisc.image
    ~file:("resources/menu/" ^ filename) () in
  let item = GMenu.image_menu_item
    ~label
    ~image:icon
    ~packing:menu#append () in
  let submenu = GMenu.menu
    ~packing:item#set_submenu () in
  let create_subitem (label, callback) =
    let subitem = GMenu.menu_item
      ~label:label
      ~packing:submenu#append () in
      subitem#connect#activate ~callback:callback; ();
  in
    List.iter create_subitem subitems

(*Create the main menu bar (a the top of the window)*)
let create_main_menubar ~packing () =
  let menu_bar = GMenu.menu_bar
    ~packing () in

  (*Déclaration du menu File*)
  let menu_file_title = GMenu.menu_item
    ~label:"File"
    ~packing:menu_bar#append () in
  let menu_file = GMenu.menu
    ~packing:menu_file_title#set_submenu () in

  (*Déclaration du menu View*)
  let menu_view_title = GMenu.menu_item
    ~label:"View"
    ~packing:menu_bar#append () in
  let menu_view = GMenu.menu
    ~packing:menu_view_title#set_submenu () in

  (*Déclaration du menu Help*)
  let menu_help_title = GMenu.menu_item
    ~label:"?"
    ~packing:menu_bar#append () in
  let menu_help = GMenu.menu
    ~packing:menu_help_title#set_submenu () in

    add_stock_item menu_file ~stock:`NEW  ~callback:void ();
    add_separator menu_file ();
    let exit_item =
      add_stock_item menu_file ~stock:`QUIT ~callback:destroy ()
    in
      exit_item#event#connect#button_release ~callback:ask_destroy;

      add_submenu menu_view
	~label:"View mode"
	~filename:"eye.png"
	~subitems:[("Free",void);
		   ("First person",void)] ();
      add_submenu menu_view
	~label:"Display mode"
	~filename:"display.png"
	~subitems:[("Wireframe",void);
		   ("Plain",void)] ();
      add_submenu menu_view
	~label:"Camera"
	~filename:"cam.png"
	~subitems:[("Rotate...",void);
		   ("Scale...",void);
		   ("Zoom...",void);
		   ("Position...",void)] ();
      add_submenu menu_view
	~label:"Light"
	~filename:"light.png"
	~subitems:[("Color...",void);
		   ("Rotate...",void);
		   ("Position",void)] ();

      add_stock_item menu_help ~stock:`ABOUT
	~callback:(!about_win#misc#show) ();
      add_stock_item menu_help ~stock:`HELP  ~callback:void ();
      menu_view_title#misc#set_sensitive false;

      menu_bar

(* --- STATUS BAR --- *)
let create_status_bar ~packing =
  let statbar = GMisc.statusbar
    ~has_resize_grip:true
    ~packing () in
    statbar#new_context ~name:"Information display"

(* ---  LablGL --- *)
let create_gl_area ~packing ?show () =
  let area = GlGtk.area
    [`RGBA;`DEPTH_SIZE 1;`DOUBLEBUFFER]
    ~width:500
    ~height:500
    ?show
    ~packing () in
    area

(* --- SIDEBAR --- *)
let create_sidebar ~packing () =
  let appercu_area = GBin.frame
    ~label:"Preview"
    ~label_xalign:0.05
    ~width:200
    ~height:210
    ~packing () in
  let appercu = GMisc.image
    ~show:false
    ~packing:appercu_area#add () in
    imgPreview := appercu;
    GPack.vbox
      ~packing:!sidebar_vbox#add ();
    let b = GButton.button
      ~label:"Compute edges"
      ~packing:(!sidebar_vbox#pack ~expand:false) () in
      b#set_image (GMisc.image ~stock:`EXECUTE ())#coerce;
      b#misc#set_sensitive false;
      b#connect#clicked	~callback:edge_image;
      sidebar_button := b;
    GPack.vbox
      ~height:10
      ~packing:(!sidebar_vbox#pack ~expand:false) ();

(* --- TOOLBAR --- *)
type tool_button =
    Button of string * string * string * (unit -> unit)
  | Separator

type toolbar = {
  bar : GButton.toolbar;
  buttons : GButton.button array;
}

let create_toolbar ~packing ~buttons ?show () =
  let toolbar = GButton.toolbar
    ~orientation:`HORIZONTAL
    ~style:`BOTH
    ~height:80
    ?show
    ~packing () in

  let add_tool_button = function
      Separator ->
	toolbar#insert_space ();
    | Button(text,icon,tooltip,cb) ->
	toolbar#insert_button
	  ~text:text
	  ~icon:(GMisc.image ~file:("resources/toolbar/" ^ icon)())#coerce
	  ~tooltip:tooltip
	  ~callback:cb();
	() in

    List.iter add_tool_button buttons;
    toolbar

(* --- STATE BUTTONS --- *)
let create_state_buttons ~packing () =
  let button1 = GButton.button
    ~label:"1 Pre-treatment"
    ~packing ()
  and button2 = GButton.button
    ~label:"2. Sampling"
    ~packing ()
  and button3 = GButton.button
    ~label:"3. 3D view"
    ~packing () in

  let buttons = [|button1;button2;button3|] in
(*    Array.iter (fun b -> b#misc#set_sensitive false) buttons;*)

    buttons

let init_state_buttons  () =
  !state_buttons.(0)#connect#clicked
    ~callback:(update_gui_status 1 );
  !state_buttons.(1)#connect#clicked
    ~callback:(update_gui_status 2 );
  !state_buttons.(2)#connect#clicked
    ~callback:(update_gui_status 3 );
  ()

let activate_state_button i ~buttons () =
  buttons.(i)#set_image (GMisc.image
			   ~file:"resources/statebar/ok.png");
  buttons.(i)#connect#pressed ~callback:void

(* --- GUI INIT --- *)
let init () =
  (* widgets witch do not change along the program execution *)
  window := create_main_window ();

  about_win := create_about_win ();

  main_vbox := GPack.vbox
    ~homogeneous:false
    ~packing:!window#add ();


  (* theses widgets may change according to status of the program *)
  menubar := create_main_menubar
    ~packing:(!main_vbox#pack ~expand:false) ();

  toolbars := [|
    (create_toolbar
       ~packing:(!main_vbox#pack ~expand:false)
       ~buttons:([
		   Button("Open image","insert-image.svg","Open an
    image file", open_fileWin);
		   Separator;
		   Button("Help","help.svg","Get helped", void)
		 ]) ());
    (create_toolbar
       ~packing:(!main_vbox#pack ~expand:false)
       ~show:false
       ~buttons:([
		   Button("Altitudes...","view-sort-ascending.svg","Modify
    altitudes you have entered", void);
		   Button("Save image","document-save.svg","Save the
    computed image file", void);
		   Separator;
		   Button("Help","help.svg","Get helped", void)
		 ]) ());
    (create_toolbar
       ~packing:(!main_vbox#pack ~expand:false)
       ~show:false
       ~buttons:([
		   Button("Save 3D model","document-save.svg","Save
    .obj file", void);
		   Separator;
		   Button("Camera mode","camera-web.svg","Take control
    of the camera", void);
		   Separator;
		   Button("Free","view-fullscreen.svg","Switch to free
    camera mode", void);
		   Button("First person","eyes.png","Switch to first
    person mode", void);
		   Separator;
		   Button("Help","help.svg","Get helped", void)
		 ]) ())|];

  program_state_hbox := GPack.hbox
    ~homogeneous:true
    ~height:34
    ~border_width:2
    ~packing:(!main_vbox#pack ~expand:false) ();


  state_buttons := create_state_buttons
	~packing:!program_state_hbox#add ();

  main_view_hbox := GPack.hbox
    ~border_width:20
    ~packing:!main_vbox#add ();

  sidebar_vbox := GPack.vbox
    ~width:200
    ~packing:(!main_view_hbox#pack ~expand:false) ();
  create_sidebar ~packing:(!sidebar_vbox#pack ~expand:false) ();

  let main_view = GBin.scrolled_window
    ~placement:`BOTTOM_RIGHT
    ~border_width:10
    ~packing:!main_view_hbox#add () in

  statusbar := create_status_bar
    ~packing:(!main_vbox#pack ~expand:false);

  main_img := GMisc.image
    ~packing:main_view#add_with_viewport
    ~show:false ();

  init_state_buttons ();
  init_about_win ();

  !window#show ();
  GMain.Main.main ()
