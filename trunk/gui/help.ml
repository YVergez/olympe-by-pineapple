(* --- CREATE & SHOW HELP DIALOG --- *)
let helpWin = ref (GWindow.window ())

let create () =
  helpWin := GWindow.window
    ~title:"Olympe - Help"
    ~modal:false
    ~position:`CENTER
    ~resizable:true
    ~width:600
    ~height:400
    ~icon:((GMisc.image ())#misc#render_icon ~size:`MENU `HELP)
    ~show:false ();

  (* Tree view *)
  let getHelpFiles path =
    (* Operations on filenames (get, sort, and clean) *)
    let dir = Unix.opendir path
    and regex = Str.regexp "h\\([0-9]+\\)_\\(.+\\)\\.html$" in
    let rec dir2list l =
      try
	let filename = Unix.readdir dir in
	  if Str.string_match regex filename 0 then
	    filename::(dir2list l)
	  else
	    dir2list l
      with End_of_file -> l
    in
    let filenames = ref (dir2list []) in
      Unix.closedir dir;
      let compare_filenames f1 f2 =
	let f1 = int_of_string (Str.global_replace regex "\\1" f1)
	and f2 = int_of_string (Str.global_replace regex "\\1" f2) in
	  f1 - f2
      and clean_filenames filename =
	Str.global_replace regex "\\2" filename
      in
	filenames := List.stable_sort compare_filenames !filenames;
	let clean_filenames = List.map clean_filenames !filenames in

	(* Getting content *)
	let record_content filename =
	  let chan = open_in ("resources/help/" ^ filename) in
	  let rec file2str str =
	    try
	      str ^ (file2str (input_line chan))
	    with End_of_file -> str
	  in
	    file2str ""
	in
	  let files_content = Array.of_list (List.map record_content !filenames) in

	    Printf.printf "%s\n%!" files_content.(0);
	    (clean_filenames,files_content)
  in

  let (filenames, files_content) = getHelpFiles "resources/help/" in

  let mainHbox = GPack.hbox
    ~packing:!helpWin#add ()
  in

  let (list_store,col) = GTree.store_of_list
    Gobject.Data.string
    filenames
  in

  let list_view = GTree.view
    ~model:list_store
    ~width:200
    ~packing:(mainHbox#pack ~expand:false) ()
  in
  let view_col = GTree.view_column
    ~renderer:(GTree.cell_renderer_text [],[("text",col)])
    ~title:"Index" ()
  in

  let displaySelectedPage path vc =
    let name = list_store#get ~row:(list_store#get_iter path) ~column:col in
      Printf.printf "Selected:%s%!" name;
  in

  (* Text buffer view *)
  let scroll_win = GBin.scrolled_window
    ~hpolicy:`NEVER
    ~vpolicy:`AUTOMATIC
    ~shadow_type:`IN
    ~packing:(mainHbox#pack ~expand:true) ()
  in

  let tag_table = GText.tag_table () in

  let text_buffer = GText.buffer
    ~tag_table:tag_table
    ~text:"EMPTY\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol\nlol" ()
  in

  let text_view = GText.view
    ~buffer:text_buffer
    ~editable:false
    ~cursor_visible:false
    ~justification:`LEFT
    ~wrap_mode:`WORD
    ~packing:(scroll_win#add) ()
  in

    ignore (list_view#append_column view_col);
    ignore (list_view#connect#row_activated ~callback:displaySelectedPage);

    text_view#set_left_margin 10;
    text_view#set_right_margin 10;
    ignore (text_buffer#create_tag ~name:"bold" [`WEIGHT(`BOLD)]);

    ignore (!helpWin#event#connect#delete ~callback:(fun ev -> !helpWin#misc#hide ();true));
    ()

let show () =
  !helpWin#misc#show ();
  ignore (!helpWin#present ())
