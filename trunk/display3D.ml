(* Oui, j'aime pourir le code des gens :p *)
let window = ref (GWindow.window ())
and gui_mode = ref false

let print_points nom_fichier =
  let file = open_in nom_fichier
  and s = ref "" in
  let rec print_p () =
    s := input_line file;
    try
      let splitted_line = Array.of_list (Str.split (Str.regexp " ") !s) in
	if !s.[0] = 'v' then
	  begin
	    print_string splitted_line.(1);
	    print_string splitted_line.(2);
	    print_string splitted_line.(3);
	    print_int (Array.length splitted_line);
	  end;
	print_p ();
    with end_of_file -> close_in file
  in print_p ()

let count_vertices_file file ref_nb =
  ref_nb := 0;
  let file_c = open_in file
  and s = ref ""  in
  let rec count_verts () =
    s := input_line file_c;
    try
      if !s.[0] = 'v' then
	ref_nb := !ref_nb + 1;
      count_verts ();
    with End_of_file -> close_in file_c
  in count_verts ()

let extract_points nom_fichier vect_ref =
  let file = open_in nom_fichier
  and s = ref ""
  and vect_count = ref 0 in
  let rec extract_p () =
    s := input_line file;
    try
      let splitted_line = Array.of_list (Str.split (Str.regexp " ") !s) in
	if ((!s.[0]) = 'v') then
	  begin
	    !vect_ref.(!vect_count) <- ((float_of_string splitted_line.(1),
					 float_of_string splitted_line.(2),
					 float_of_string splitted_line.(3)):Gl.point3);
	    vect_count := !vect_count + 1;
	  end;
	extract_p ()
    with end_of_file -> close_in file
  in extract_p ()

let count_faces_file filename ref_nb =
  ref_nb := 0;
  let file = open_in filename
  and s = ref ""  in
  let rec count_faces () =
    s := input_line file;
    try
      if !s.[0] = 'f' then
	ref_nb := !ref_nb + 1;
      count_faces ();
    with End_of_file -> close_in file
  in count_faces ()

let extract_faces filename faces_ref =
  let file = open_in filename
  and s = ref ""
  and faces_count = ref 0 in
  let rec extract_f () =
    s := input_line file;
    try
      let splitted_line = Array.of_list (Str.split (Str.regexp " ") !s) in
	if ((!s.[0]) = 'f') then
	  begin
	    !faces_ref.(!faces_count) <- (int_of_string splitted_line.(1),
					  int_of_string splitted_line.(2),
					  int_of_string splitted_line.(3));
	    faces_count := !faces_count + 1;
	  end;
	extract_f ()
    with end_of_file -> close_in file
  in extract_f ()

let print_triplet t =
  let (a,b,c) = t in
    Printf.printf "%i %i %i \n" a b c

let print_vect3_tab tab =
  for i = 0 to ((Array.length tab) - 1) do
    print_triplet tab.(i);
    print_string "\n";
  done

let print_quadruplet t =
  let (a,b,c,d) = t in
    print_int a;
    print_int b;
    print_int c;
    print_int d

let print_vect4_tab tab =
  for i = 0 to ((Array.length tab) - 1) do
    print_quadruplet tab.(i);
    print_string "\n";
  done

let make_unique_points vect_array_ref =
  let i = ref 0
  and res = ref [] in
    while !i < Array.length !vect_array_ref do
      if not (List.exists (fun x -> x = !vect_array_ref.(!i)) !res) then
	res := !vect_array_ref.(!i)::!res;
      i := !i + 1;
    done;
    Array.of_list !res

let find_pos_vect x vect =
  let i = ref 0 in
    while (!i < Array.length vect) && (x <> vect.(!i)) do
      i := !i + 1;
    done;
    if !i >= Array.length vect then
      failwith "non trouvé"
    else
      !i

let organise_faces faces points unique_points =
  let i = ref 0 in
    while !i < Array.length faces do
      let (a,b,c) = faces.(!i) in
	faces.(!i) <- (find_pos_vect points.(a-1) unique_points +
			 1,find_pos_vect points.(b-1) unique_points + 1,find_pos_vect
			   points.(c-1) unique_points + 1);
	i := !i + 1;
    done

let antialiasing points =
  let i = ref 0
  and j = ref 0
  and nb_p = ref 0
  and temp_z = ref 0. in
    while !i < Array.length points do
      j := 0;
      nb_p := 0;
      let (x,y,z) = points.(!i) in
	temp_z := z;
	while !j < Array.length points do
	  if !j <> !i then
	    begin
	      let (a,b,c) = points.(!j) in
		if (abs_float (x-.a) <= 15.) && (abs_float (y-.b) <= 15.) then
		  begin
		    temp_z := !temp_z +. c;
		    nb_p := !nb_p + 1
		  end;
	    end;
	  j := !j + 1;
	done;
	points.(!i) <- (x,y,!temp_z/.float !nb_p);
	i := !i + 1;
    done

let enable () =
  Gl.enable `depth_test

let light () =
  Gl.enable `lighting;
  Gl.enable `color_material;
  Gl.enable `light0;
  GlLight.light ~num:0 (`ambient (1.0,1.0,1.0,1.0))

let render area vect_array faces_array draw_mode xrot yrot xpos ypos zpos () =
  GlClear.color ~alpha:1.0 (1.0, 1.0, 1.0);
  GlClear.clear [`color;`depth];
  enable ();
  GlDraw.shade_model `smooth;
  (*GlFunc.blend_func `src_alpha `one_minus_src_alpha;*)
  GlMat.load_identity ();
  GlMat.translate ~z:(-10.0) ~y:(-.5.0) ();
  GlMat.rotate ~angle:!xrot ~x:1.0 ();
  GlMat.rotate ~angle:!yrot ~y:1.0 ();
  GlMat.translate ~x:(-.(!xpos)) (*~y:(-.(!ypos))*) ~z:(-.(!zpos)) ();
  GlMat.push ();
  GlMat.translate ~z:5.0 ();
  for i = 0 to ((Array.length !faces_array) - 1) do
    let (a,b,c) = !faces_array.(i) in
      let ((x,y,z):Gl.point3) = (vect_array.(a-1))
      and ((x2,y2,z2):Gl.point3) = (vect_array.(b-1))
      and ((x3,y3,z3):Gl.point3) = (vect_array.(c-1)) in
	GlDraw.begins draw_mode;
(*	Printf.printf "%f %f %f" z z2 z3;*)
	GlDraw.color ~alpha:1.0 (0.0,0.0,(-.z +. 10.) /. 60.);
	GlDraw.vertex3 (y/.10.0,-.z/.10.0,x/.10.0);
	(*GlDraw.color ~alpha:1.0 (0.0,0.0,(mod_float (z2/.(-50.0)) 1.0) +.0.4);*)
	GlDraw.color ~alpha:1.0 (0.0,0.0,(-.z2 +. 10.) /. 60.);
	GlDraw.vertex3 (y2/.10.0,-.z2/.10.0,x2/.10.0);
	GlDraw.color ~alpha:1.0 (0.0,0.0,(-.z3 +. 10.) /. 60.);
	GlDraw.vertex3 (y3/.10.0,-.z3/.10.0,x3/.10.0);
	GlDraw.ends ();
  done;
  GlMat.pop ();
  area#swap_buffers ()
  (*Glut.swapBuffers ();
  Glut.setCursor Glut.CURSOR_NONE*)

let reshape ~width ~height =
  GlDraw.viewport ~x:0 ~y:0 ~w:width ~h:height;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.perspective ~fovy:60.0 ~aspect:(1.0 *. float width /. float height) ~z:(0.1,100.0);
  GlMat.mode `modelview

let determine_draw_mode = function
    "-w" -> (`line_loop:GlDraw.shape)
  | "-f" -> (`triangles:GlDraw.shape)
  | _ -> invalid_arg "unknown option"

let keyboard xrot yrot xpos ypos zpos ev =
  let key = GdkEvent.Key.keyval ev in
  let char_key =
    try char_of_int key
    with Invalid_argument "char_of_int" -> '>' (* if the key is not a letter (arrow, etc...)*)
  in
    if key = 27 then
      GMain.Main.quit ();
    match char_key with
	'a' -> xrot := (!xrot +. 1.0);
	  if !xrot > 360.0 then
	    xrot := !xrot-.360.0
      | 'w' -> xrot := (!xrot -. 1.0);
	  if !xrot < -360.0 then
	    xrot := !xrot+.360.0
      | 'z' -> let yrotrad = (!yrot /. 180.0 *. 3.141592654)
	       and xrotrad = (!xrot /. 180.0 *. 3.141592654) in
	  xpos := !xpos +. (sin yrotrad);
	  zpos := !zpos -. (cos yrotrad);
	  ypos := !ypos -. (sin xrotrad)
      | 's' -> let yrotrad = (!yrot /. 180.0 *. 3.141592654)
	       and xrotrad = (!xrot /. 180.0 *. 3.141592654) in
	  xpos := !xpos -. (sin yrotrad);
	  zpos := !zpos +. (cos yrotrad);
	  ypos := !ypos +. (sin xrotrad)
      | 'd' -> let yrotrad = (!yrot /. 180.0 *. 3.141592654) in
	  xpos := !xpos +. cos(yrotrad);
	  zpos := !zpos +. sin(yrotrad);
      | 'q' -> let yrotrad = (!yrot /. 180.0 *. 3.141592654) in
	  xpos := !xpos -. cos(yrotrad);
	  zpos := !zpos -. sin(yrotrad);
      | _ -> ()

let mouse_movement lastx lasty xrot yrot ev =
  let x = GdkEvent.Motion.x_root ev
  and y = GdkEvent.Motion.y_root ev in
  let diffx = x -. !lastx
  and diffy = y -. !lasty in
    lastx := x;
    lasty := y;
    xrot := !xrot +. diffy /. 7.0;
    yrot := !yrot +. diffx /. 7.0

(* Toogle cursor appearence (hided/regular) *)
let toogle_hide_cursor =
  let hided = ref false in
    function win ->
      if not !hided then
	begin
	  let create_null_cursor () =
	    let w, h = 1, 1 in
	    let mask = Gdk.Bitmap.create ~window:win ~width:w ~height:h () in
	    let pixmap = Gdk.Pixmap.create ~window:win ~width:w ~height:h ~depth:1 () in
	    let color = Gdk.Color.alloc (Gdk.Color.get_system_colormap ()) (`RGB (0, 0, 0)) in
	    Gdk.Cursor.create_from_pixmap pixmap mask color color w h
	  in
	    Gdk.Window.set_cursor win (create_null_cursor ())
	end
      else
	Gdk.Window.set_cursor win (Gdk.Cursor.create `ARROW);
      hided := not !hided

let init ?box () =
  (*ignore( Glut.init Sys.argv );*)
  (*Glut.initDisplayMode ~double_buffer:true ~depth:true ();
  Glut.initWindowSize ~w:800 ~h:600 ;
  ignore (Glut.createWindow ~title:"Olympe");*)

  if not !gui_mode then
    begin
      let win = GWindow.window
	~width:800
	~height:600
	~title:"Olympe" ()
      in
	ignore (win#connect#destroy
		  ~callback:GMain.Main.quit);
	window := win;
    end;

  let glArea = match box with
      Some box -> box
    | None ->
	GlGtk.area
	  [`RGBA;`DEPTH_SIZE 1;`DOUBLEBUFFER]
	  ~packing:!window#add ()
  in
    GlClear.color ~alpha:1.0 (1.0, 1.0, 1.0);
    GlClear.clear [`color;`depth];

    glArea

let draw_map mode ?(gui=false) ?win ?box ?allow filename =
  gui_mode := gui;

  (* If in GUI mode, we get the main window *)
  (match win with
       Some w -> window := w
     | None -> ());

  (* If in CLI mode, we have to accept inputs anytime *)
  let allow_inputs = match allow with
       Some b -> b
     | None -> ref true
  in

  let nb_vects = ref 0
  and nb_faces = ref 0
  and file = filename
  and draw_mode = (determine_draw_mode mode)
  and xrot = ref 0.0
  and yrot = ref 0.0
  and xpos = ref 0.0
  and ypos = ref 0.0
  and zpos = ref 10.0
  and lastx = ref 0.0
  and lasty = ref 0.0 in
    count_vertices_file file nb_vects;
    count_faces_file file nb_faces;
    let vect_array_ref = ref (Array.make !nb_vects ((0.,0.,0.):Gl.point3))
    and faces_array_ref = ref (Array.make !nb_faces (0,0,0)) in
      extract_points file vect_array_ref;
      extract_faces file faces_array_ref;
      let uvar = make_unique_points vect_array_ref in
	organise_faces !faces_array_ref !vect_array_ref uvar;
	antialiasing uvar;

	let glArea = init ?box () in

	  (*Glut.gameModeString "1680x1050:32@75";
	  Glut.enterGameMode ();
	  Glut.keyboardFunc ~cb:(keyboard xrot yrot xpos ypos zpos);
	  Glut.displayFunc ~cb:(render uvar faces_array_ref
				  draw_mode xrot yrot xpos ypos zpos);
	  Glut.reshapeFunc ~cb:reshape;
	  Glut.idleFunc ~cb:(Some (render uvar faces_array_ref
				     draw_mode xrot yrot xpos ypos zpos ));
	  Glut.passiveMotionFunc ~cb:(mouse_movement lastx lasty xrot yrot);
	  Glut.mainLoop ()*)

	  let render_param =
	    render glArea uvar faces_array_ref draw_mode
	      xrot yrot xpos ypos zpos
	  and keyboard_param =
	    keyboard xrot yrot xpos ypos zpos
	  and mouse_mov_param =
	    mouse_movement lastx lasty xrot yrot
	  in


	  let id_display =
	    glArea#connect#display
	      ~callback:(render_param)
	  and id_reshape =
	    glArea#connect#reshape
	      ~callback:reshape
	  and id_keyboard =
	    !window#event#connect#key_press
	      ~callback:(fun ev ->
			   if !allow_inputs then
			     begin
			       keyboard_param ev;
			       render_param ();
			     end;
			   false)
	  and id_mouse =
	    !window#event#connect#motion_notify
	      ~callback:(fun ev ->
			   if !allow_inputs then
			     begin
			       mouse_mov_param ev;
			       render_param ();
			     end;
			   false)
	  in
	    ignore (glArea#event#add [`ALL_EVENTS]);

	  (if not !gui_mode then
	     begin
	       !window#show ();
	       GMain.Main.main ()
	     end);

	  (* Return ids to disconnect callbacks later *)
	  [|[id_display;id_reshape];[id_keyboard;id_mouse]|]
