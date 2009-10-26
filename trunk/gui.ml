let useless () = ()

let destroy () =
  GMain.Main.quit ()

let window = GWindow.window
  ~title:"Olympe pre-beta GUI" ()

let main_vbox = GPack.vbox
  ~packing:window#add ()

let create_simple_item menu label callback =
  let item = GMenu.menu_item
      ~label:label
      ~packing:menu#append () in
    item#connect#activate ~callback:callback

let create_separator_item menu =
  GMenu.separator_item
    ~packing:menu#append ()

let create_submenu_item menu label values =
  let submenu_item = GMenu.menu_item
    ~label:label
    ~packing:menu#append () in
  let submenu = GMenu.menu
    ~packing:submenu_item#set_submenu () in
  let create_subitem (label, callback) =
    create_simple_item submenu label callback;();
  in
    List.iter create_subitem values

let create_main_menubar () =
  let menu_bar = GMenu.menu_bar
    ~height:20
    ~packing:main_vbox#add () in

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

  (*Déclaration du menu About*)
  let menu_about_title = GMenu.menu_item 
    ~label:"About"
    ~packing:menu_bar#append () in
  let menu_about = GMenu.menu
    ~packing:menu_about_title#set_submenu () in
  
  (*Déclaration du menu Help*)
  let menu_help_title = GMenu.menu_item 
    ~label:"Help"
    ~packing:menu_bar#append () in
  let menu_help = GMenu.menu
    ~packing:menu_help_title#set_submenu () in
    create_simple_item    menu_file  "New project..."  useless;
    create_simple_item    menu_file  "Open project..." useless;
    create_simple_item    menu_file  "Save project..." useless;
    create_separator_item menu_file;
    create_simple_item    menu_file  "Quit"            destroy;

    create_submenu_item   menu_view  "View mode"      [("Free",useless);
						       ("First person",useless)];
    create_submenu_item   menu_view  "Display mode"   [("Wireframe",useless);
						       ("Plain",useless)];
    create_submenu_item   menu_view  "Camera"         [("Rotate...",useless);
						       ("Scale...",useless);
						       ("Zoom...",useless);
						       ("Position...",useless)];
    create_submenu_item   menu_view  "Light"          [("Color...",useless);
						       ("Rotate...",useless);
						       ("Position",useless)];

    create_simple_item    menu_about "About..." useless;

    create_simple_item    menu_help  "Get help..." useless
    

let main () =
  window#connect#destroy ~callback:destroy;
  create_main_menubar ();
  window#show ();
  GMain.Main.main ()

let _ = main ()
