let toolbar1 = ref (GButton.toolbar ())
and toolbar2 = ref (GButton.toolbar ())
and toolbar3 = ref (GButton.toolbar ())
and toolbar_unhide = ref (GButton.button ())
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

(* Toogle hide/show GUI *)
let toogleGuiDisplay =
  let showed = ref true in
    fun () ->
      if !showed then
	begin
	  (* We hide GUI *)
	  Skel.toolbar_vbox#misc#hide ();
	  Skel.statebar_hbox#misc#hide ();
	  Skel.sidebar_vbox#misc#hide ();
	  Skel.toolbar_unhide_vbox#misc#show ();
	  showed := false;
	end
      else
	begin
	  (* We show GUI *)
	  Skel.toolbar_vbox#misc#show ();
	  Skel.toolbar_unhide_vbox#misc#hide ();
	  Skel.statebar_hbox#misc#show ();
	  Skel.sidebar_vbox#misc#show ();
	  showed := true;
	end

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

(* Set draw mode value *)
let setDrawMode mode () =
  if mode = `triangles then
    Skel.draw_mode := `line_loop
  else
    Skel.draw_mode := `triangles;
  toogleDrawMode ()

(* Toogle camera mode value (true/false) *)
let toogleCameraMode () =
  Skel.camera_mode := not !Skel.camera_mode;
  ignore ((Mainview.get3DViewArea ())#event#send (GdkEvent.create `FOCUS_CHANGE))

let setCameraMode mode () =
  if mode = "free" then
    Skel.camera_mode := true
  else if mode = "object" then
    Skel.camera_mode := false
  else
    invalid_arg ("setCameraMode : Unknown mode \"" ^ mode ^ "\"");
  toogleCameraMode ()

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
		       Help.show)
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
		       Help.show)
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
		Button("Filled",
		       "plain.png",
		       "Display model in filled faces mode",
		       toogleDrawMode);
		Button("Change camera mode",
		       "camera.png",
		       "Switch camera mode between free and object",
		       toogleCameraMode);
		Separator;
		Button("Hide GUI",
		       "view-fullscreen.svg",
		       "Hide GUI to have 3D view in fullscreen",
		       toogleGuiDisplay);
		Separator;
		Button("Help",
		       "help.svg",
		       "Get helped",
		       Help.show)
		 ]) ()
  in
  let tbh = GButton.button
    ~label:"Show GUI"
    ~packing:Skel.toolbar_unhide_vbox#add ()
  in
    toolbar1 := tb1;
    toolbar2 := tb2;
    toolbar3 := tb3;
    ignore (tbh#connect#clicked ~callback:toogleGuiDisplay);
    toolbar_unhide := tbh;
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

