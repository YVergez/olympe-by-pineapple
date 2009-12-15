val statusbar_context : GMisc.statusbar_context ref
val create : unit -> unit
val clean : unit -> unit
val setInfo : ?timeout:bool -> string -> unit
val getContext : unit -> GMisc.statusbar_context ref
