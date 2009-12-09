let toolbar1 = ref (GButton.toolbar ())
and toolbar2 = ref (GButton.toolbar ())
and toolbar3 = ref (GButton.toolbar ())
and allow_inputs = ref false

(* Hide nth item in toolbar t *)
let hideInToolbar t n =
  let tb = match t with
      1 -> !toolbar1
    | 2 -> !toolbar2
    | _ -> !toolbar3 in
  (Array.of_list tb#children).(n)#misc#hide ()

(* Hide nth item in toolbar t *)
let showInToolbar t n =
  let tb = match t with
      1 -> !toolbar1
    | 2 -> !toolbar2
    | _ -> !toolbar3 in
  (Array.of_list tb#children).(n)#misc#show ()

(* Toogle draw_mode value (wireframe/plain) *)
let toogleDrawMode () =
  if !Skel.draw_mode = `triangles then
    (hideInToolbar 3 2;
     showInToolbar 3 3;
     Skel.draw_mode := `line_loop)
  else
    (hideInToolbar 3 3;
     showInToolbar 3 2;
     Skel.draw_mode := `triangles);
  ignore ((Mainview.get3DViewArea ())#event#send (GdkEvent.create `FOCUS_CHANGE))

(* Define a simple variant type for easily create toolbars *)
type tool_button =
    Button of string * string * string * (unit -> unit)
  | ToggleButton of string * string * string * (unit -> unit)
  | Separator

(* Create a complete toolbars (with buttons) *)
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
	ignore (toolbar#insert_button
	  ~text:text
	  ~icon:(GMisc.image ~file:("resources/toolbar/" ^ icon)())#coerce
	  ~tooltip:tooltip
	  ~callback:cb());
    | ToggleButton(text,icon,tooltip,cb) ->
	ignore (toolbar#insert_toggle_button
	  ~text:text
	  ~icon:(GMisc.image ~file:("resources/toolbar/" ^ icon)())#coerce
	  ~tooltip:tooltip
	  ~callback:cb());
	() in

    List.iter add_tool_button buttons;
    toolbar

(* Constructor of toolbars *)
let create () =
  let tb1 = create_toolbar
    ~packing:Skel.toolbar_vbox#add
    ~buttons:([
		Button("Open image",
		       "insert-image.svg",
		       "Open an image file",
		       Dialogs.showOpenFile);
		Separator;
		Button("Help",
		       "help.svg",
		       "Get helped",
		       Dialogs.showHelp)
	      ]) ()
  in
  let tb2 = create_toolbar
    ~packing:Skel.toolbar_vbox#add
    ~show:false
    ~buttons:([
		Button("Altitudes...",
		       "view-sort-ascending.svg",
		       "Modify altitudes you have entered",
		       Skel.showDialogAltitudes);
		Button("Save image",
		       "document-save.svg",
		       "Save the computed image file",
		       (Dialogs.showSaveFile Dialogs.IMAGE));
		Separator;
		Button("Help",
		       "help.svg",
		       "Get helped",
		       Dialogs.showHelp)
		 ]) ()
  in
  let tb3 = create_toolbar
    ~packing:Skel.toolbar_vbox#add
    ~show:false
    ~buttons:([
		Button("Save 3D model",
		       "document-save.svg",
		       "Save .obj file",
		       (Dialogs.showSaveFile Dialogs.OBJ));
		Separator;
		ToggleButton("Activate keyboard",
		       "input-keyboard.svg",
		       "Take control of the camera",
		       Skel.toogleAllowInputs);
		Separator;
		Button("Wireframe",
		       "wireframe.png",
		       "Display model in wireframe mode",
		       toogleDrawMode);
		Button("Plain",
		       "plain.png",
		       "Display model in plain faces mode",
		       toogleDrawMode);
		Separator;
		Button("Help",
		       "help.svg",
		       "Get helped",
		       Dialogs.showHelp)
		 ]) ()
  in
    toolbar1 := tb1;
    toolbar2 := tb2;
    toolbar3 := tb3;
    hideInToolbar 3 3

(* Used to get a pointer on one of the 3 toolbars. Useful for showing/hidding toolbars *)
let get = function
    0 -> !toolbar1
  | 1 -> !toolbar2
  | 2 -> !toolbar3
  | n -> failwith ("Invalid call to Toolbar.get : " ^ (string_of_int n) ^ "th element do not exist.")

(* --- Enable Drag and Drop features in main view zone --- *)
let initFileDnD () =
  let receiveImageDrop drag_context ~x ~y data ~info ~time =
    Dialogs.verifyFile [Skel.cleanFilename data#data];
  in

    Skel.mainview_vbox#drag#dest_set
      ~flags:[`ALL]
      ~actions:[`MOVE]
      [{Gtk.target = "text/uri-list"; flags=[]; info=0}];
    ignore (Skel.mainview_vbox#drag#connect#data_received
	      ~callback:receiveImageDrop)

