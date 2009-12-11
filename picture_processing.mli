val closed_pix : Sdlvideo.surface -> int -> int -> int -> int -> bool
val del_ext : string -> string
val incr_list : ('a * 'b * 'c * int) list -> ('a * 'b * 'c * int) list
val rec_color : (int * int * int * int) list -> string -> unit
val rf_cur_list : 'a -> 'a list -> 'a list
val process_img : string -> string -> string * int * (int * int * int) list
