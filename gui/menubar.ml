let menubar = ref (GMenu.menu_bar ())

(* Clear all values and loaded files *)
let restartProgram () =
  Skel.map_file :=  "";
  Skel.edged_file :=  "resources/tmp/edged.bmp";
  Skel.obj_file :=  "resources/tmp/map.obj";
  Skel.colors_alt :=  [(0,0,0,0)];
  Skel.step :=  30;
  Skel.allow_inputs :=  false;
  Skel.display_ids :=  ([||]:(GtkSignal.id list) array);
  Skel.use_edges :=  true;

  Mainview.setMainInfoText ("Open an image to render using the \"Open image\"" ^
    "button in the toolbar or drag and drop an image file here.");
  Mainview.setMainInfoImg "resources/toolbar/insert-image.svg";
  Statebar.moveToState 0 ();
  Mainview.showMainInfoView ();
  !Statebar.statebar_button1#misc#set_sensitive false;
  !Statebar.statebar_button2#misc#set_sensitive false;
  !Statebar.statebar_button3#misc#set_sensitive false

(* New project confirmation box *)
let showNewConfirmBox ev =
  let win = GWindow.message_dialog
    ~message:("Are you sure you want to restart\nprogram with a new map ?\n\n" ^
		"You will lose all unsaved changes.")
    ~message_type:`WARNING
    ~buttons:GWindow.Buttons.yes_no
    ~parent:(Skel.getWindow ())
    ~destroy_with_parent:true
    ~title:"Are you sure ?"
    ~modal:false
    ~position:`CENTER_ON_PARENT
    ~resizable:false ()
  in
    match win#run () with
	`NO -> win#destroy ()
      | _ -> win#destroy ();restartProgram ()


(* Tools for creating menubar *)
(*Add a item from stocks to menu*)
let add_stock_item menu ~stock ~callback () =
  let item = GMenu.image_menu_item
    ~stock
    ~packing:menu#append () in
    ignore (item#connect#activate ~callback);
    item

(*Add a separator item to menu*)
let add_separator menu () =
  ignore (GMenu.separator_item
	    ~packing:menu#append ())

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
      ignore (subitem#connect#activate ~callback:callback)
  in
    List.iter create_subitem subitems

(* Menubar creator *)
let create () =
  let menu_bar = GMenu.menu_bar
    ~packing:(Skel.menubar_vbox#pack ~expand:false) () in

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

    ignore (add_stock_item menu_file ~stock:`NEW  ~callback:showNewConfirmBox ());
    let exit_item =
      add_stock_item menu_file ~stock:`QUIT ~callback:Skel.exitProgram ()
    in
      ignore (exit_item#event#connect#button_release ~callback:Skel.askExit);

      add_submenu menu_view
	~label:"View mode"
	~filename:"eye.png"
	~subitems:[("Free",Toolbar.setCameraMode "free");
		   ("Object",Toolbar.setCameraMode "object")] ();
      add_submenu menu_view
	~label:"Display mode"
	~filename:"display.png"
	~subitems:[("Wireframe",Toolbar.setDrawMode `line_loop);
		   ("Plain",Toolbar.setDrawMode `triangles)] ();

      ignore (add_stock_item menu_help ~stock:`ABOUT
	~callback:(Dialogs.showAbout) ());
      ignore (add_stock_item menu_help ~stock:`HELP  ~callback:Help.show ());
      menu_view_title#misc#set_sensitive false;

      menubar := menu_bar;
      Skel.menubar := menu_bar
