(* --- SHOW ERROR --- *)
let showErrMessage ?(title="Error") ~text () =
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
	  if ext = ".bmp" || ext = ".jpg" || ext = ".jpeg"
	    || ext = ".png" || ext = ".gif"  then
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

(* Create filter on .bmp *)
let img_filter = GFile.filter
  ~name:"Image file"
  ~patterns:["*.bmp";"*.jpeg";"*.jpg";"*.png";"*.gif"] ()

(* Create filter on .obj *)
let obj_filter = GFile.filter
  ~name:"OBJ 3D model files"
  ~patterns:["*.obj"] ()

let showOpenFile () =
  let open_window = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~parent:(Skel.getWindow ())
    ~destroy_with_parent:true
    ~title:"Choose an image file"
    ~modal:true
    ~position:`CENTER_ON_PARENT
    ~resizable:true
    ~show:false () in
    open_window#add_select_button_stock `OK `VALID;

    open_window#add_filter img_filter;

    (match open_window#run () with
	 `VALID -> verifyFile ~callback:open_window#destroy open_window#get_filenames
       | `DELETE_EVENT -> open_window#destroy ()
       | _ -> ())


(* --- SAVE FILE DIALOG --- *)
type file = IMAGE | OBJ

let saveFile location file_type =
  let filename =
    try Skel.cleanFilename location
    with Skel.IsADirectory ->
      showErrMessage
	~title:"Can't save the file !"
	~text:"The file you specified is a directory" ();
	""
  in

    (match file_type with
	 IMAGE -> Skel.copyFile !Skel.edged_file filename
       | OBJ   -> Skel.copyFile !Skel.obj_file filename)

let showSaveFile file_type () =
  let title =
    match file_type with
	IMAGE -> "the edged map image file"
      | OBJ -> "the .obj file"
  in

  let win = GWindow.file_chooser_dialog
    ~action:`SAVE
    ~parent:(Skel.getWindow ())
    ~destroy_with_parent:true
    ~title:("Save " ^ title)
    ~allow_grow:true
    ~allow_shrink:true
    ~modal:true
    ~position:`CENTER_ON_PARENT
    ~resizable:true ()
  in

    win#add_select_button_stock `SAVE `SAVE;
    (match file_type with
	IMAGE -> win#add_filter img_filter;
	  win#set_current_name "my_edged_map.bmp";
      | OBJ -> win#add_filter obj_filter;
	  win#set_current_name "my_3D_model.obj");
    win#set_show_hidden false;

    match win#run () with
	`SAVE ->
	  saveFile (List.hd win#get_uris) file_type;
	  win#destroy ();
      | _ -> win#destroy ()


(* --- SELECT GRID'S COLOR --- *)
let showColorSelector () =
  let win = GWindow.color_selection_dialog
    ~title:"Select a grid color"
    ~parent:(Skel.getWindow ())
    ~destroy_with_parent:true
    ~modal:true
    ~position:`CENTER_ON_PARENT ()
  in

    match win#run () with
	`OK -> win#destroy ()
      | _ -> win#destroy ()
