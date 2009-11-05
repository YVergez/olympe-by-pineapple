(* Define a record to have access to all components of the gui *)
type gui =
{
  mutable img_src      : string;
  mutable pre_img_src  : string;
  mutable inter        : int;
  mutable obj_file_src : string;
  mutable prog_state   : int;

  window : GWindow.window;
  main_vbox : GPack.box;
  menubar : GMenu.menu_shell;
  toolbars : GButton.toolbar array;
  program_state_hbox : GPack.box;
  state_buttons : GButton.button array;
  main_view_hbox : GPack.box;
  sidebar_vbox : GPack.box;
  statusbar : GMisc.statusbar_context;
  about_win : GWindow.about_dialog;
  main_img : GMisc.image;
  gl_area : GlGtk.area;
}

(*An useless fonction*)
let void () = ()

(*Terminate application*)
let destroy () =
  GMain.Main.quit ()

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
    ~resizable:false
    ~show:true () in
    match ask_dialog#run () with
      | `YES ->
	  destroy (); (*bricolage pour gérer la fermeture à partir du menu*)
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

let create_about_win ~window () =
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
    ~parent:window
    ~destroy_with_parent:true
    ~modal:false
    ~position:`CENTER_ON_PARENT
    ~resizable:false
    ~show:false () in
    win

let init_about_win ~gui () =
  let hide_about ev =
    gui.about_win#misc#hide ()
  and event_hide_about ev =
    gui.about_win#misc#hide ();
    true
  in
    gui.about_win#connect#response ~callback:hide_about;
    gui.about_win#event#connect#delete
      ~callback:event_hide_about;
    ()

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

(*Create the main menu bar (a the top of the window*)
let create_main_menubar ~packing ~about_win () =
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
	~callback:(about_win#misc#show) ();
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
    ~width:200
    ~height:200
    ~border_width:5
    ~packing () in
  let appercu = GMisc.image
    ~show:false
    ~packing:appercu_area#add () in
    ()

(* --- TOOLBAR --- *)
type tool_button =
    Button of string * string * string
  | Separator

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
    | Button(text,icon,tooltip) ->
	toolbar#insert_button
	  ~text:text
	  ~icon:(GMisc.image ~file:("resources/toolbar/" ^ icon) ())#coerce
	  ~tooltip:tooltip ();
	() in

    List.iter add_tool_button buttons;
    toolbar

(* --- CHANGING GUI APPEARANCE --- *)
let rec update_gui_status state ~gui () =
  match state with
      0 -> update_gui_status 1 ~gui ();
    | 1 ->
	(Array.of_list gui.menubar#children).(1)#misc#set_sensitive false;
	gui.toolbars.(0)#misc#show ();
	gui.toolbars.(1)#misc#hide ();
	gui.toolbars.(2)#misc#hide ();
    | 2 ->
	(Array.of_list gui.menubar#children).(1)#misc#set_sensitive false;
	gui.toolbars.(0)#misc#hide ();
	gui.toolbars.(1)#misc#show ();
	gui.toolbars.(2)#misc#hide ();
    | 3 ->
	(Array.of_list gui.menubar#children).(1)#misc#set_sensitive true;
	gui.toolbars.(0)#misc#hide ();
	gui.toolbars.(1)#misc#hide ();
	gui.toolbars.(2)#misc#show ();
    | _ -> ()


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

let init_state_buttons ~gui () =
  gui.state_buttons.(0)#connect#clicked
    ~callback:(update_gui_status 1 ~gui);
  gui.state_buttons.(1)#connect#clicked
    ~callback:(update_gui_status 2 ~gui);
  gui.state_buttons.(2)#connect#clicked
    ~callback:(update_gui_status 3 ~gui);
  ()

let activate_state_button i ~buttons () =
  buttons.(i)#set_image (GMisc.image
			   ~file:"resources/statebar/ok.png");
  buttons.(i)#connect#pressed ~callback:void

(* --- GUI INIT --- *)
let init () =
  (* widgets witch do not change along the program execution *)
  let window = create_main_window () in

  let about_win = create_about_win ~window () in

  let main_vbox = GPack.vbox
    ~homogeneous:false
    ~packing:window#add () in

  (* theses widgets may change according to status of the program *)
  let menubar = create_main_menubar
    ~packing:(main_vbox#pack ~expand:false)
    ~about_win:about_win () in

  let toolbars = [|
    (create_toolbar
       ~packing:(main_vbox#pack ~expand:false)
       ~buttons:([
		   Button("Open image","insert-image.svg","Open an
    image file");
		   Separator;
		   Button("Help","help.svg","Get helped")
		 ]) ());
    (create_toolbar
       ~packing:(main_vbox#pack ~expand:false)
       ~show:false
       ~buttons:([
		   Button("Altitudes...","view-sort-ascending.svg","Modify
    altitudes you have entered");
		   Button("Save image","document-save.svg","Save the
    computed image file");
		   Separator;
		   Button("Help","help.svg","Get helped")
		 ]) ());
    (create_toolbar
       ~packing:(main_vbox#pack ~expand:false)
       ~show:false
       ~buttons:([
		   Button("Save 3D model","document-save.svg","Save .obj file");
		   Separator;
		   Button("Camera mode","camera-web.svg","Take control of the camera");
		   Separator;
		   Button("Free","view-fullscreen.svg","Switch to free
    camera mode");
		   Button("First person","eyes.png","Switch to first
    person mode");
		   Separator;
		   Button("Help","help.svg","Get helped")
		 ]) ())|]
  in

  let program_state_hbox = GPack.hbox
    ~homogeneous:true
    ~height:34
    ~border_width:2
    ~packing:(main_vbox#pack ~expand:false) () in

  let state_buttons = create_state_buttons
    ~packing:program_state_hbox#add () in

  let main_view_hbox = GPack.hbox
    ~packing:main_vbox#add () in

  let sidebar_vbox = GPack.vbox
    ~width:220
    ~border_width:10
    ~packing:(main_view_hbox#pack ~expand:false) () in
    create_sidebar ~packing:(sidebar_vbox#pack ~expand:false) ();

  let statusbar = create_status_bar
    ~packing:(main_vbox#pack ~expand:false) in

  (* Declaration of 3 components of main view *)
  let main_img = GMisc.image
    ~packing:main_view_hbox#add
    ~show:false ()
  and gl_area = create_gl_area
    ~packing:main_view_hbox#add
    ~show:false () in

  let guiWidgets : gui = {
    img_src = "";
    pre_img_src = "resources/tmp/pre_img.bmp";
    inter = 42;
    obj_file_src = "resources/tmp/final_obj.obj";
    prog_state = 0;

    window = window;
    main_vbox = main_vbox;
    menubar = menubar;
    toolbars = toolbars;
    program_state_hbox = program_state_hbox;
    state_buttons = state_buttons;
    main_view_hbox = main_view_hbox;
    sidebar_vbox = sidebar_vbox;
    statusbar = statusbar;
    about_win = about_win;
    main_img = main_img;
    gl_area = gl_area;
  } in

    init_state_buttons ~gui:guiWidgets ();
    init_about_win ~gui:guiWidgets ();
    
    window#show ();
