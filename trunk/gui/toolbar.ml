let toolbar1 = ref (GButton.toolbar ())
and toolbar2 = ref (GButton.toolbar ())
and toolbar3 = ref (GButton.toolbar ())


(* Define a simple variant type for easily create toolbars *)
type tool_button =
    Button of string * string * string * (unit -> unit)
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
		       Skel.void);
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
		       Skel.void);
		Separator;
		Button("Camera mode",
		       "camera-web.svg",
		       "Take control of the camera",
		       Skel.void);
		Separator;
		Button("Free",
		       "view-fullscreen.svg",
		       "Switch to free camera mode",
		       Skel.void);
		Button("First person",
		       "eyes.png",
		       "Switch to first person mode",
		       Skel.void);
		Separator;
		Button("Help",
		       "help.svg",
		       "Get helped",
		       Dialogs.showHelp)
		 ]) ()
  in
    toolbar1 := tb1;
    toolbar2 := tb2;
    toolbar3 := tb3

(* Used to get a pointer on one of the 3 toolbars. Useful for showing/hidding toolbars *)
let get = function
    0 -> !toolbar1
  | 1 -> !toolbar2
  | 2 -> !toolbar3
  | n -> failwith ("Invalid call to Toolbar.get : " ^ (string_of_int n) ^ "th element do not exist.")
