val statusbar : GMisc.statusbar ref
val statusbar_context : GMisc.statusbar_context ref
val last_timeout_id : Glib.Timeout.id ref
val create : unit -> unit
val cleanByTimeout : Gtk.statusbar_message -> unit -> bool
val clean : unit -> unit
val setInfo : ?timeout:bool -> string -> unit
val getContext : unit -> GMisc.statusbar_context ref
