val draw_map :
  string ->
  ?gui:bool ->
  ?win:GWindow.window ->
  ?box:GlGtk.area ->
  ?allow:bool ref ->
  ?d_mode:GlDraw.shape ref ->
  colors:(int * int * int * int) list ->
  ?statusbar:< pop : unit -> 'a; push : string -> 'b; .. > ref ->
  ?camera_rotating:bool ref ->
  ?sidebar_img:GMisc.image ref ->
  ?map_file:string -> string -> GtkSignal.id list array
