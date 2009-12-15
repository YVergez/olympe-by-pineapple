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
