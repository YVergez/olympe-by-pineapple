val main_map_img : GMisc.image ref
val main_map_edged_img : GMisc.image ref
val main_map : GBin.scrolled_window ref
val main_map_edged : GBin.scrolled_window ref
val main_3d_view : GlGtk.area ref
val main_info_view : GPack.box
val main_info_text : GMisc.label
val main_info_img : GMisc.image
val grid_color : Gdk.color ref
val setMainInfoText : string -> unit
val setMainInfoImg : string -> unit
val setMainInfoImgByStock : GtkStock.id -> unit
val init : unit -> unit
val setMainMapImg : string -> unit
val refreshStepOnEdgedImg : unit -> unit
val setMainMapEdgedImg : 'a -> unit
val showMainMap : unit -> unit
val showMainMapEdged : unit -> unit
val showMain3DView : unit -> unit
val showMainInfoView : unit -> unit
val new3DViewArea : unit -> unit
val get3DViewArea : unit -> GlGtk.area
