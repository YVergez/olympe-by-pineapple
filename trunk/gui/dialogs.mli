val showErrMessage : ?title:string -> text:'a -> unit -> unit
val showAbout : unit -> unit
val verifyFile : ?callback:(unit -> unit) -> string list -> unit
val showOpenFile : unit -> unit
type file = IMAGE | OBJ
val saveFile : string -> file -> unit
val showSaveFile : file -> unit -> unit
val showColorSelector : unit -> unit
