(* --- MISC -- *)
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
    ~deletable:false
    ~focus_on_map:true
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

(*Create the main window*)
let create_main_window () =
  let window = GWindow.window
    ~position:`CENTER
    ~width:800
    ~height:600
    ~resizable:true
    ~allow_grow:true
    ~allow_shrink:true
    ~title:"Olympe pre-beta GUI" () in
    window#event#connect#delete ~callback:ask_destroy;
    window#event#connect#destroy ~callback:ask_destroy;
    window#connect#destroy ~callback:destroy;
    window

(*Create an image from a image file*)
let create_menu_icon_from_file ~file () =
  let img = GMisc.image
    ~file () in
    img

(* --- MENU BAR --- *)
(*Add a simple text item to a menu*)
let create_text_item menu label callback () =
  let item = GMenu.menu_item
    ~label:label
    ~packing:menu#append () in
    item#connect#activate ~callback;
    item

(*Add a simple image & text item to a menu*)
let create_image_item menu label callback ~file () =
  let menu_icon = create_menu_icon_from_file
    ~file:("resources/menu/icn/" ^ file) () in
  let item = GMenu.image_menu_item
    ~label:label
    ~image:menu_icon
    ~packing:menu#append () in 
    item#connect#activate ~callback;
    item

(*Add a separator item to a menu*)
let create_separator_item menu () =
  GMenu.separator_item
    ~packing:menu#append ()

(*Add an item with multiple choices to a menu*)
let create_submenu_item menu label values () =
  let submenu_item = GMenu.menu_item
    ~label:label
    ~packing:menu#append () in
  let submenu = GMenu.menu
    ~packing:submenu_item#set_submenu () in
  let create_subitem (label, callback) =
    create_text_item submenu label callback ();();
  in
    List.iter create_subitem values

(*Add an image item with multiple choices to a menu*)
let create_submenu_image_item menu label values ~file () =
  let icon = GMisc.image
    ~file:("resources/menu/icn/" ^ file) () in
  let submenu_item = GMenu.image_menu_item
    ~label:label
    ~image:icon
    ~packing:menu#append () in
  let submenu = GMenu.menu
    ~packing:submenu_item#set_submenu () in
  let create_subitem (label, callback) =
    create_text_item submenu label callback ();();
  in
    List.iter create_subitem values

(*Create the main menu bar (a the top of the window*)
let create_main_menubar ~packing ~window =
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

    create_image_item    menu_file  "New project..."  void ~file:"new.png" ();
    create_image_item    menu_file  "Open project..." void ~file:"open.png" ();
    create_image_item    menu_file  "Save project..." void ~file:"save.png" ();
    create_separator_item menu_file ();
    let exit_item = create_image_item    menu_file  "Quit" void
      ~file:"exit.png" () in
    (*really quit ?*)
      exit_item#event#connect#button_release ~callback:ask_destroy;

    create_submenu_image_item   menu_view  "View mode"   
      [("Free",void);
       ("First person",void)] ~file:"eye.png" ();
    create_submenu_image_item   menu_view  "Display mode"  
      [("Wireframe",void);
       ("Plain",void)] ~file:"display.png" ();
    create_submenu_image_item   menu_view  "Camera"        
      [("Rotate...",void);
       ("Scale...",void);
       ("Zoom...",void);
       ("Position...",void)] ~file:"cam.png" ();
    create_submenu_image_item   menu_view  "Light"    
      [("Color...",void);
       ("Rotate...",void);
       ("Position",void)] ~file:"light.png" ();

    create_image_item    menu_help "About..." void ~file:"about.png" ();
    create_image_item    menu_help  "Get help..." void ~file:"help.png" ()

(* --- STATUS BAR --- *)
let create_status_bar ~packing =
  let statbar = GMisc.statusbar
    ~has_resize_grip:true
    ~packing () in
    statbar#new_context ~name:"Information display"

(* ---  LablGL --- *)
let create_gl_area ~packing () =
  let area = GlGtk.area
    [`RGBA;`DEPTH_SIZE 1;`DOUBLEBUFFER]
    ~width:500
    ~height:500 
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
    ~file:"resources/sidebar/appercu.bmp"
    ~packing:appercu_area#add () in
    appercu#clear ()
    

(* --- GUI INIT --- *)
let init () =
  let window = create_main_window () in

  let main_vbox = GPack.vbox
    ~homogeneous:false
    ~packing:window#add () in

  let main_menu_bar = create_main_menubar
    ~packing:(main_vbox#pack ~expand:false) ~window:window in

  let toolbar_hbox = GPack.hbox
    ~height:60
    ~packing:(main_vbox#pack ~expand:false) () in

  let program_state_hbox = GPack.hbox
    ~height:10
    ~packing:(main_vbox#pack ~expand:false) () in

  let main_view_hbox = GPack.hbox
    ~packing:main_vbox#add () in

  let sidebar_vbox = GPack.vbox
    ~width:220
    ~border_width:10
    ~packing:(main_view_hbox#pack ~expand:false) () in
    create_sidebar ~packing:(sidebar_vbox#pack ~expand:false) ();

  let status_bar = create_status_bar
    ~packing:(main_vbox#pack ~expand:false) in

    status_bar#push "Adding new status...";
    window#show ();
