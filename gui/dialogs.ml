(* --- SHOW HELP DIALOG --- *)
let showHelp () =
  ()


(* --- SHOW ABOUT DIALOG --- *)
let licence2string file =
  let file = open_in file in
  let rec cat_in result =
    try
      cat_in (result ^ (input_line file) ^ "\n")
    with End_of_file -> close_in file; result
  in
    cat_in ""

let showAbout () =
  let win = GWindow.about_dialog
    ~authors:
    ["Guillaume Algis [algis_g@epita.fr]";
     "Perrine Brunel [brunel_a@epita.fr]";
     "Hugo Damme [damme_h@epita.fr]";
     "Yohann Vergez [vergez_y@epita.fr]"]
    ~copyright:"Copyright 2009 Pineapple"
    ~license:(licence2string "resources/gpl-3.0.txt")
    ~logo:((GMisc.image ~file:"resources/olympe-logo.png" ())#pixbuf)
    ~name:"Olympe"
    ~title:"Olympe"
    ~version:"1.2"
    ~website:"http://olympe-pineapple.fr.nf/"
    ~website_label:"Olympe developpers' blog"
    ~parent:(Skel.getWindow ())
    ~destroy_with_parent:true
    ~modal:false
    ~position:`CENTER_ON_PARENT
    ~resizable:false
    ~show:false ()
  in
    match win#run () with
	_ -> win#destroy ()

(* --- SHOW OPEN FILE DIALOG --- *)
(* Verify the output of the open file dialog and update gui *)
let verifyFile ?(callback=Skel.void) filenames =
  (match filenames with
      [] -> ()
    | filename::_ ->
	let ext =
	  try
	    String.sub filename (String.rindex filename '.') 4
	  with Not_found -> "DIR"
	in
	  if ext = ".bmp" then
	    begin
	      Skel.setMapFile filename;
	      Mainview.setMainMapImg filename;
	      Mainview.showMainMap ();
	      Sidebar.changeAppercuImgByPixbuf
		(Skel.createMiniature
		   ~filename:filename
		   ~width:180
		   ~height:180 ());
	      Sidebar.setMainButtonSensitive true 0;
	    end);
  callback ()

let showOpenFile () =
  let open_window = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~parent:(Skel.getWindow ())
    ~destroy_with_parent:true
    ~title:"Choose an image file"
    ~icon:(GMisc.image ~file:"resources/toolbar/insert-image.svg" ())#pixbuf
    ~modal:true
    ~position:`CENTER_ON_PARENT
    ~resizable:true
    ~show:false () in
    open_window#add_select_button_stock `OK `VALID;

    (* Adding filter on .bmp *)
    let filter = GFile.filter
      ~name:"BMP files"
      ~patterns:["*.bmp"] ()
    in
      open_window#add_filter filter;

      (match open_window#run () with
	   `VALID -> verifyFile ~callback:open_window#destroy open_window#get_filenames
	 | `DELETE_EVENT -> open_window#destroy ()
	 | _ -> ())
