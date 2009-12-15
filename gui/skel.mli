exception IsADirectory
val window : GWindow.window ref
val map_file : string ref
val edged_file : string ref
val obj_file : string ref
val colors_alt : (int * int * int * int) list ref
val step : int ref
val menubar : GMenu.menu_shell ref
val allow_inputs : bool ref
val camera_mode : bool ref
val display_ids : GtkSignal.id list array ref
val draw_mode : GlDraw.shape ref
val posx : float ref
val posy : float ref
val posz : float ref
val rotx : float ref
val roty : float ref
val getMapFile : unit -> string
val setMapFile : string -> unit
val getEdgedFile : unit -> string
val setEdgedFile : string -> unit
val getObjFile : unit -> string
val setObjFile : string -> unit
val createMiniature :
  width:int -> height:int -> filename:string -> unit -> GdkPixbuf.pixbuf
val toogleAllowInputs : unit -> unit
val copyFile : string -> string -> unit
val cleanFilename : string -> string
val void : unit -> unit
val exitProgram : unit -> unit
val main_vbox : GPack.box ref
val menubar_vbox : GPack.box
val toolbar_vbox : GPack.box
val toolbar_unhide_vbox : GPack.box
val statebar_hbox : GPack.box
val side_and_main_hbox : GPack.box
val sidebar_vbox : GPack.box
val mainview_vbox : GPack.box
val statusbar_hbox : GPack.box
val askExit : 'a -> bool
val windowCreate : unit -> unit
val getWindow : unit -> GWindow.window
val showOnlyChild :
  int ->
  < all_children : < misc : < show : unit -> 'a; .. >; .. > list;
    children : < misc : < hide : unit -> unit; .. >; .. > list; .. > ->
  'a
val setMenuSensitive : int -> bool -> unit
val createAltFile : unit -> unit
val showDialogAltitudes : unit -> unit
