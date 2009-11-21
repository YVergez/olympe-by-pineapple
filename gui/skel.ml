(* Define useful functions *)
(* ... or not *)
let void () = ()

(* Exit the program *)
let exitProgram () =
  GMain.Main.quit ()


(* --- MAIN STRUCTURE --- *)
(* Create all containers of the program's widgets *)
let main_vbox = ref (GPack.vbox
  ~homogeneous:false ())
let menubar_vbox = GPack.vbox
  ~homogeneous:false
  ~packing:(!main_vbox#pack ~expand:false) ()
and toolbar_vbox = GPack.vbox
  ~homogeneous:false
  ~packing:(!main_vbox#pack ~expand:false) ()
and statebar_hbox = GPack.hbox
  ~homogeneous:false
  ~packing:(!main_vbox#pack ~expand:false) ()
and sidebar_vbox = GPack.vbox
  ~homogeneous:false
  ~packing:(!main_vbox#pack ~expand:false) ()
and mainview_vbox = GPack.vbox
  ~homogeneous:false
  ~packing:(!main_vbox#pack ~expand:false) ()
and statusbar_hbox = GPack.hbox
  ~homogeneous:false
  ~packing:(!main_vbox#pack ~expand:false) ()


(* --- MAIN WINDOW STRUCTURE --- *)
let window = ref (GWindow.window())

(* Create a dialog asking the user if he really wanna quit.
It is the only dialog not implemented in Dialogs module because
it is called directly by the window (window's delete event).*)
let askExit ev =
  let ask_dialog = GWindow.message_dialog
    ~modal:true
    ~message:"Do you really want to exit the application ?"
    ~use_markup:true
    ~message_type:`INFO
    ~buttons:GWindow.Buttons.yes_no
    ~destroy_with_parent:true
    ~title:"Quit ?"
    ~position:`CENTER_ON_PARENT
    ~resizable:false ()
  in
    match ask_dialog#run () with
      | `YES ->
	  exitProgram ();
	  false
      | _    ->
	  ask_dialog#destroy ();
	  true

(* Main window creator *)
let windowCreate () =
  let win = GWindow.window
    ~position:`CENTER
    ~width:800
    ~height:600
    ~resizable:true
    ~allow_grow:true
    ~allow_shrink:true
    ~icon:((GMisc.image ~file:"resources/icon.png" ())#pixbuf)
    ~title:"Olympe 1.2" () in
    (*win#maximize ();*)
    ignore (win#event#connect#delete ~callback:askExit);
    ignore (win#event#connect#destroy ~callback:askExit);
    ignore (win#connect#destroy ~callback:exitProgram);
    win#add (!main_vbox#coerce);
    window := win

let windowGet () =
  !window
