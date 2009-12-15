val menubar : GMenu.menu_shell ref
val restartProgram : unit -> unit
val showNewConfirmBox : 'a -> unit
val add_stock_item :
  < append : GMenu.menu_item -> unit; .. > ->
  stock:GtkStock.id ->
  callback:(unit -> unit) -> unit -> GMenu.image_menu_item
val add_separator : < append : GMenu.menu_item -> unit; .. > -> unit -> unit
val add_submenu :
  < append : GMenu.menu_item -> unit; .. > ->
  label:string ->
  filename:string -> subitems:(string * (unit -> unit)) list -> unit -> unit
val create : unit -> unit
