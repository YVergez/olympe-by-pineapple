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
	  let buffer = GText.buffer
	    ~tag_table:(GText.tag_table ())
	    ~text:"" ()
	  in
	  ignore (buffer#create_tag ~name:"html" []);
	  ignore (buffer#create_tag ~name:"p" [`INDENT(20)]);
	  ignore (buffer#create_tag ~name:"title"
		    [`WEIGHT(`BOLD);`SIZE_POINTS(23.);
		     `FOREGROUND_GDK(Gdk.Color.alloc
				       ~colormap:(Gdk.Color.get_system_colormap ())
				       (`NAME("#2b8fad")))]);
	  ignore (buffer#create_tag ~name:"h1"
		    [`WEIGHT(`BOLD);`SIZE_POINTS(16.)]);
	  ignore (buffer#create_tag ~name:"h2"
		    [`WEIGHT(`BOLD);`SIZE_POINTS(14.)]);
	  ignore (buffer#create_tag ~name:"h3"
		    [`WEIGHT(`BOLD);`SIZE_POINTS(12.)]);
	  ignore (buffer#create_tag ~name:"strong"
		    [`WEIGHT(`BOLD)]);
	  ignore (buffer#create_tag ~name:"em"
		    [`STYLE(`ITALIC)]);

	    (* Lexing file *)
	    let xml = Xml.parse_file ("resources/help/" ^ filename) in
	    let modify_buffer_by_tag = function
		"p" -> buffer#insert "\n\n"
	      | "title" | "h1" | "h2" | "h3" -> buffer#insert "\n"
	      | _ -> ()
	    in
	    let rec xml2buffer = function
		Xml.Element(name,_,children) ->
		  let start =  buffer#create_mark (buffer#end_iter) in
		  let stop  =  buffer#create_mark ~left_gravity:false (buffer#end_iter) in
		    List.iter xml2buffer children;
		    modify_buffer_by_tag name;
		    buffer#apply_tag_by_name name
		      ~start:(buffer#get_iter_at_mark (`MARK(start)))
		      ~stop:(buffer#get_iter_at_mark (`MARK(stop)));
	      | Xml.PCData(data) ->
		  buffer#insert data;
	    in
	      xml2buffer xml;
	      buffer
	in
	let files_content = Array.of_list (List.map record_content !filenames) in

	    (* Return ... *)
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
    ~title:"Table of content" ()
  in

  (* Text buffer view *)
  let scroll_win = GBin.scrolled_window
    ~hpolicy:`NEVER
    ~vpolicy:`AUTOMATIC
    ~shadow_type:`IN
    ~packing:(mainHbox#pack ~expand:true) ()
  in

  let text_buffer = GText.buffer
    ~tag_table:(GText.tag_table ())
    ~text:("\n\n<-- Please select a page in the left pane " ^
    "by double-clicking on its name") ()
  in

  let text_view = GText.view
    ~buffer:text_buffer
    ~editable:false
    ~cursor_visible:false
    ~justification:`LEFT
    ~wrap_mode:`WORD
    ~packing:(scroll_win#add) ()
  in

  let displaySelectedPage path vc =
    let name = list_store#get ~row:(list_store#get_iter path) ~column:col in
    let i = ref 0 in
    let rec get_list_pos x = function
	[] -> -1
      | h::t when h = x -> !i
      | _::t -> i := !i + 1; get_list_pos x t
    in
    let pos = get_list_pos name filenames in
      text_view#set_buffer files_content.(pos)
  in

    if Array.length files_content <> 0 then
      text_view#set_buffer files_content.(0);
    ignore (list_view#append_column view_col);
    ignore (list_view#connect#row_activated ~callback:displaySelectedPage);

    text_view#set_left_margin 10;
    text_view#set_right_margin 10;

    ignore (!helpWin#event#connect#delete ~callback:(fun ev -> !helpWin#misc#hide ();true));
    ()

let show () =
  !helpWin#misc#show ();
  ignore (!helpWin#present ())
