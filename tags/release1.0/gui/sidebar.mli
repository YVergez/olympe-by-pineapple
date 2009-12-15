val appercu1 : GMisc.image ref
val appercu2 : GMisc.image ref
val appercu3 : GMisc.image ref
val sidebar1 : GPack.box ref
val sidebar2 : GPack.box ref
val sidebar3 : GPack.box ref
val sidebar1_button : GButton.button ref
val sidebar2_button : GButton.button ref
val edgeImage : unit -> unit
val create3dModel : unit -> unit
val createAppercu : int -> unit -> GBin.frame
val createSidebar1 : unit -> unit
val updateStep : < value : float; .. > -> unit -> unit
val showColorSelector : unit -> unit
val createSidebar2 : unit -> unit
val createSidebar3 : unit -> unit
val create : unit -> unit
val changeAppercuImg : string -> unit
val changeAppercuImgByPixbuf : GdkPixbuf.pixbuf -> unit
val setMainButtonSensitive : bool -> int -> unit
val get : int -> GPack.box
