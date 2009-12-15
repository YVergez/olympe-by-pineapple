val toolbar1 : GButton.toolbar ref
val toolbar2 : GButton.toolbar ref
val toolbar3 : GButton.toolbar ref
val toolbar_unhide : GButton.button ref
val allow_inputs : bool ref
val hideInToolbar : int -> int -> unit
val showInToolbar : int -> int -> unit
val toogleGuiDisplay : unit -> unit
val toogleDrawMode : unit -> unit
val setDrawMode : [> `triangles ] -> unit -> unit
val toogleCameraMode : unit -> unit
val setCameraMode : string -> unit -> unit
type tool_button =
    Button of string * string * string * (unit -> unit)
  | ToggleButton of string * string * string * (unit -> unit)
  | Separator
val create_toolbar :
  packing:(GObj.widget -> unit) ->
  buttons:tool_button list -> ?show:bool -> unit -> GButton.toolbar
val create : unit -> unit
val get : int -> GButton.toolbar
val initFileDnD : unit -> unit
