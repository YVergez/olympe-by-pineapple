exception IsADirectory

(* The main window reference *)
let window = ref (GWindow.window())

(* Infos on files we treat *)
let map_file = ref ""
and edged_file = ref "resources/tmp/edged.bmp"
and obj_file = ref "resources/tmp/map.obj"
and colors_alt = ref [(0,0,0,0)]
and step = ref 30
and menubar = ref (GMenu.menu_bar ())
and allow_inputs = ref false
and display_ids = ref ([||]:(GtkSignal.id list) array)
and use_edges = ref true

let getMapFile () =
  !map_file

let setMapFile file =
  map_file := file

let getEdgedFile () =
  !edged_file

let setEdgedFile file =
  edged_file := file

let getObjFile () =
  !obj_file

let setObjFile file =
  obj_file := file

(* Create a miniature pixel buffer (image) from filename *)
let createMiniature ~width ~height ~filename () =
  GdkPixbuf.from_file_at_size
    filename
    width
    height

(* Toogle allow_inputs value (true/false) *)
let toogleAllowInputs () =
  allow_inputs := not !allow_inputs

(* Copy a file to destination *)
let copyFile source dest =
  let in_chan = open_in source
  and out_chan = open_out dest in
  let rec cp =
    try output_string out_chan (input_line in_chan)
    with End_of_file -> ()
  in
    cp;
    close_in in_chan;
    close_out out_chan

(* Clean a filename of type "file://location\n" *)
let cleanFilename filename =
  let newFilename =
      ref (Str.global_replace (Str.regexp "file://") "" (String.escaped filename))
  in
  let bs_pos =
    try String.index !newFilename '\\'
    with Not_found -> (-1) (* If we didn't found a backslash, good for us ! *)
  in
    if bs_pos <> (-1) then
      newFilename := String.sub !newFilename 0 (bs_pos);

    if !newFilename.[(String.length !newFilename) - 1] = '/' then
      raise IsADirectory; (* If string ends with a '/' it's a dir *)
    !newFilename

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
  ~homogeneous:true
  ~packing:(!main_vbox#pack ~expand:false) ()
and side_and_main_hbox = GPack.hbox
  ~homogeneous:false
  ~packing:(!main_vbox#pack ~expand:true) ()
let sidebar_vbox = GPack.vbox
  ~homogeneous:false
  ~width:220
  ~border_width:10
  ~packing:(side_and_main_hbox#pack ~expand:false) ()
and mainview_vbox = GPack.vbox
  ~homogeneous:false
  ~border_width:10
  ~packing:(side_and_main_hbox#pack ~expand:true) ()
and statusbar_hbox = GPack.hbox
  ~homogeneous:false
  ~packing:(!main_vbox#pack ~expand:false) ()


(* --- MAIN WINDOW STRUCTURE --- *)
(* Create a dialog asking the user if he really wanna quit.
It is the only dialog not implemented in Dialogs module because
it is called directly by the window (window's delete event).*)
let askExit ev =
  let ask_dialog = GWindow.message_dialog
    ~modal:true
    ~message:"Do you really want to exit the application ?"
    ~use_markup:true
    ~message_type:`WARNING
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

let getWindow () =
  !window

let showOnlyChild n obj =
  List.iter (fun o -> o#misc#hide ()) obj#children;
  ((Array.of_list obj#all_children).(n))#misc#show ()

let setMenuSensitive n sens =
  (Array.of_list !menubar#all_children).(n)#misc#set_sensitive sens


(* --- SHOW A DIALOG WITCH ASK ALTITUDES TO USER --- *)
let showDialogAltitudes () =
  let dialog = GWindow.dialog
    ~parent:!window
    ~destroy_with_parent:true
    ~title:"Altitudes"
    ~allow_grow:false
    ~allow_shrink:false
    ~modal:true
    ~position:`CENTER_ON_PARENT
    ~resizable:false
    ~border_width:10
    ~width:350
    ~height:500 ()
  in
    dialog#add_button_stock `OK `VALID;
    dialog#vbox#set_homogeneous false;

    ignore (GMisc.label
	~text:("Please complete the altitude fields\ncorresponding to the following colors :\n")
	~packing:(dialog#vbox#pack ~expand:false) ());

    (* Convert a rgb triple of int into an rgba int32 *)
    let rgb2int r g b =
      let r = Int32.shift_left (Int32.of_int r) 24
      and g = Int32.shift_left (Int32.of_int g) 16
      and b = Int32.shift_left (Int32.of_int b) 8 in
	Int32.logor (Int32.logor r g) b in

    let scroll = GBin.scrolled_window
      ~hpolicy:`NEVER
      ~vpolicy:`AUTOMATIC
      ~shadow_type:`NONE
      ~packing:(dialog#vbox#pack ~expand:true) () in
    let scroll_box = GPack.vbox
      ~packing:(scroll#add_with_viewport) () in

    (* Used to carry user's altitudes *)
    let user_alts = Array.make (List.length !colors_alt) (GEdit.spin_button ()) in

    let rec print_colors i = function
      [] -> ()
      | (r,g,b,_)::t ->
	  begin
	    let block = GPack.hbox
	      ~height:50
	      ~border_width:10
	      ~packing:(scroll_box#add) () in

	    (* Spacer  30px lenght*)
	    let _ = GPack.vbox
	      ~width:30
	      ~packing:(block#pack ~expand:false) () in

	    let img = GMisc.image
	      ~pixbuf:(GdkPixbuf.create
		   ~width:90
		   ~height:30
		   ~colorspace:`RGB
		   ~has_alpha:false ())
	      ~packing:(block#pack ~expand:false) ()
	    in
	      GdkPixbuf.fill img#pixbuf (rgb2int r g b);

	      (* Spacer  30px lenght*)
	      let _ = GPack.vbox
		~width:30
		~packing:(block#pack ~expand:false) () in

	      let spin = GEdit.spin_button
		~adjustment:( GData.adjustment ~page_size:0.0 ())
		~digits:0
		~update_policy:`IF_VALID
		~value:(float_of_int (i * 10))
		~packing:(block#pack ~expand:false) () in

		user_alts.(i - 1) <- spin;
		print_colors (i + 1) t
	  end
    in
      print_colors 1 !colors_alt;
      (match dialog#run () with
	  `VALID ->
	    let rec put_alt i = function
		[] -> []
	      | (r,g,b,_)::t -> (r,g,b,(-1) * user_alts.(i)#value_as_int)::(put_alt (i + 1) t)
	    in
	      colors_alt := put_alt 0 !colors_alt;
	      dialog#destroy ()
	| _ -> dialog#destroy ())
