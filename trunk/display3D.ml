(* Oui, j'aime pourir le code des gens :p *)

let window = ref (GWindow.window ())
and gui_mode = ref false
and reniew_coord = ref false

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
    Printf.printf "%f %f %f \n" a b c

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

let rec get_color_from_alt alt = function
    [] -> (0., 0., 0.)
  | (r,g,b,al)::t when alt <= (float_of_int al) ->
	((float_of_int r)/.255., (float_of_int g)/.255., (float_of_int b)/.255.)
  | _::t -> get_color_from_alt alt t

let record_colors vect_array faces_array col_alt colors =
  for i = 0 to ((Array.length faces_array) - 1) do
    let (a,b,c) = faces_array.(i)
    and gcfa = get_color_from_alt in (*Sexy Caml*)
    let ((x,y,z):Gl.point3) = (vect_array.(a-1))
    and ((x2,y2,z2):Gl.point3) = (vect_array.(b-1))
    and ((x3,y3,z3):Gl.point3) = (vect_array.(c-1)) in
      colors.(i * 3)     <- gcfa z  col_alt;
      colors.(i * 3 + 1) <- gcfa z2 col_alt;
      colors.(i * 3 + 2) <- gcfa z3 col_alt;
  done

let render area max_vect_array vect_array faces_array draw_mode colors_array
    xrot yrot xpos ypos zpos camera () =
  GlMat.mode `modelview;
  GlClear.color ~alpha:1.0 (1.0, 1.0, 1.0);
  GlClear.clear [`color;`depth];
  enable ();
  GlDraw.shade_model `smooth;
  GlMat.load_identity ();
  if not (!camera) then
    begin
      GlMat.rotate ~angle:!xrot ~x:1.0 ();
      GlMat.rotate ~angle:!yrot ~y:1.0 ();
    end;
  GlMat.translate ~x:(-.(!xpos)) ~y:(-.(!ypos)-.10.) ~z:(-.(!zpos)) ();
  GlMat.push ();
  if !camera then
    begin
      GlMat.rotate ~angle:!xrot ~x:1.0 ();
      GlMat.rotate ~angle:!yrot ~y:1.0 ();
    end;
  for i = 0 to ((Array.length !faces_array) - 1) do
    let (a,b,c) = !faces_array.(i) in
    let ((x,y,z):Gl.point3) = (vect_array.(a-1))
    and ((x2,y2,z2):Gl.point3) = (vect_array.(b-1))
    and ((x3,y3,z3):Gl.point3) = (vect_array.(c-1)) in
      GlDraw.begins !draw_mode;
      GlDraw.color ~alpha:1.0 colors_array.(i * 3);
      GlDraw.vertex3 (y/.10.0,z/.10.0,x /. 10.0);
      GlDraw.color ~alpha:1.0 colors_array.(i * 3 + 1);
      GlDraw.vertex3 (y2/.10.0,z2/.10.0,x2/.10.0);
      GlDraw.color ~alpha:1.0 colors_array.(i * 3 + 2);
      GlDraw.vertex3 (y3/.10.0,z3/.10.0,x3/.10.0);
      GlDraw.ends ();
  done;
  GlMat.pop ();
  GluMat.look_at ~eye:(0.,10.,0.) ~center:(0.,0.,0.) ~up:(0.,1.,0.);
  area#swap_buffers ()

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

let keyboard xrot yrot xpos ypos zpos camera ev =
  let key = GdkEvent.Key.keyval ev in
  let char_key =
    try char_of_int key
    with Invalid_argument "char_of_int" -> '>' (* if the key is not a letter (arrow, etc...)*)
  in
    if key = 27 && not !gui_mode then
      GMain.Main.quit ();
    if !camera then
      match char_key with
	| 'z' ->let xrotrad = (!xrot /. 180. *.3.141592654) in
	    zpos := !zpos -. (cos xrotrad);
	    ypos := !ypos +. (sin xrotrad);
	| 's' -> let xrotrad = (!xrot /. 180. *.3.141592654) in
	    zpos := !zpos +. (cos xrotrad);
	    ypos := !ypos -. (sin xrotrad);
	| 'd' -> xpos := !xpos +. 1.;
	| 'q' -> xpos := !xpos -. 1.;
	| _ -> ()
    else
      match char_key with
	| 'z' -> let yrotrad = (!yrot /. 180. *. 3.141592654)
		 and xrotrad = (!xrot /. 180. *. 3.141592654) in
	    xpos := !xpos +. (sin yrotrad);
	    zpos := !zpos -. (cos yrotrad);
	    ypos := !ypos -. (sin xrotrad);
	| 's' -> let yrotrad = (!yrot /. 180. *. 3.141592654)
		 and xrotrad = (!xrot /. 180. *. 3.141592654) in
	    xpos := !xpos -. (sin yrotrad);
	    zpos := !zpos +. (cos yrotrad);
	    ypos := !ypos +. (sin xrotrad);
	| 'd' -> let yrotrad = (!yrot /. 180. *. 3.141592654) in
	    xpos := !xpos +.(cos yrotrad);
	    zpos := !zpos +.(sin yrotrad);
	| 'q' -> let yrotrad = (!yrot /. 180. *. 3.141592654) in
	    xpos := !xpos -.(cos yrotrad);
	    zpos := !zpos -.(sin yrotrad);
	| _ -> ()

let mouse_movement lastx lasty xrot yrot x y =
  let x = (float_of_int x)
  and y = (float_of_int y) in
    if !reniew_coord then
      reniew_coord := false
    else
      (let diffx = x -. !lastx
       and diffy = y -. !lasty in
	 xrot := !xrot +. diffy /. 7.0;
	 yrot := !yrot -. diffx /. 7.0;);
    lastx := x;
    lasty := y

let scroll_movement xrot yrot xpos ypos zpos camera ev =
  let dir = GdkEvent.Scroll.direction ev in
    if !camera then
    (match dir with
	 `UP -> zpos := !zpos -. 1.;
       | `DOWN -> zpos := !zpos +. 1.;
       | _ -> ())
    else
      (match dir with
	   `UP ->
	     let yrotrad = (!yrot /. 180.0 *. 3.141592654)
	     and xrotrad = (!xrot /. 180.0 *. 3.141592654) in
	       xpos := !xpos +. (sin yrotrad);
	       zpos := !zpos -. (cos yrotrad);
	       ypos := !ypos -. (sin xrotrad);
	 | `DOWN ->
	     let yrotrad = (!yrot /. 180.0 *. 3.141592654)
	     and xrotrad = (!xrot /. 180.0 *. 3.141592654) in
	       xpos := !xpos -. (sin yrotrad);
	       zpos := !zpos +. (cos yrotrad);
	       ypos := !ypos +. (sin xrotrad)
	 | _ -> ())

let init ?box () =
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

    glArea#drag#source_set
      ~modi:[`BUTTON1]
      ~actions:[`DEFAULT]
      [{Gtk.target = "move"; flags=[]; info=42}];
    glArea#drag#dest_set
      ~flags:[`MOTION]
      ~actions:[`DEFAULT]
      [{Gtk.target = "move"; flags=[]; info=42}];
    glArea#drag#source_set_icon
      (GDraw.pixmap
	 ~mask:true
	 ~width:1
	 ~height:1 ());
    glArea

let init_points max_points =
  let i = ref 0
  and res = Array.make (Array.length max_points) (0.,0.,0.) in
    while !i < Array.length max_points do
      let (x,y,z) = max_points.(!i) in
	res.(!i) <- (x,y,0.);
	i := !i + 1
    done;
    res

let rec refresh_points max_points points =
  let continue = ref false in
  let i = ref 0 in
    while !i < Array.length points do
      let (maxx,maxy,maxz) = max_points.(!i)
      and (x,y,z) = points.(!i) in
	if (abs_float z) < (abs_float maxz) then
	  begin
	    points.(!i) <- (x,y,z +. maxz /. 100.);
	    continue := true;
	  end;
	i := !i + 1;
    done;
    !continue

let calculate_center points =
  let center = ref (0.,0.,0.)
  and i = ref 0 in
    while !i < Array.length points do
      let (x,y,z) = points.(!i)
      and (mx,my,mz) = !center in
	center := (mx+.x,my+.y,mz+.z);
	i := !i + 1;
    done;
    let (x,y,z) = !center in
      (x/.float (Array.length points),y/.float (Array.length
  points),z/.float (Array.length points))

let calculate_max points =
  let maximum = ref (0.,0.,0.)
  and i = ref 0 in
    while !i < Array.length points do
      let (x,y,z) = points.(!i)
      and (mx,my,mz) = !maximum in
	maximum := (max x mx,max y my,0.);
	i := !i + 1;
    done;
    !maximum

let recenter points ncenter ocenter =
  let i = ref 0
  and (cx,cy,cz) = ncenter
  and (ox,oy,oz) = ocenter in
    while !i < Array.length points do
      let (x,y,z) = points.(!i) in
	points.(!i) <- (x-.ox+.cx,y-.oy+.cy,z);
	i := !i + 1;
    done

let draw_map mode ?(gui=false) ?win ?box ?allow ?d_mode ~colors filename =
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

  let draw_mode = match d_mode with
      Some m -> m
    | None -> ref (determine_draw_mode mode)
  in

  let nb_vects = ref 0
  and nb_faces = ref 0
  and file = filename
  (*and xrot = ref 51.3
  and yrot = ref 92.7
  and xpos = ref (-5.5)
  and ypos = ref 21.9
  and zpos = ref 30.9*)
  and xrot = ref 0.0
  and yrot = ref 0.0
  and xpos = ref 0.0
  and ypos = ref 0.0
  and zpos = ref 0.0
  and lastx = ref 0.0
  and lasty = ref 0.0
  and camera = ref false in
    count_vertices_file file nb_vects;
    count_faces_file file nb_faces;
    let vect_array_ref = ref (Array.make !nb_vects ((0.,0.,0.):Gl.point3))
    and faces_array_ref = ref (Array.make !nb_faces (0,0,0)) in
      extract_points file vect_array_ref;
      extract_faces file faces_array_ref;
      let uvar = make_unique_points vect_array_ref in
	organise_faces !faces_array_ref !vect_array_ref uvar;

	let colors_array = Array.make (Array.length !vect_array_ref) (1.,1.,1.) in
	record_colors uvar !faces_array_ref colors colors_array;

	antialiasing uvar;

	let center = calculate_center uvar in
	  recenter uvar (0.,0.,0.) center;
	  let t_points = init_points uvar in

	let glArea = init ?box () in
	let render_param =
	  render glArea uvar t_points faces_array_ref draw_mode
	    colors_array xrot yrot xpos ypos zpos camera
	and keyboard_param =
	  keyboard xrot yrot xpos ypos zpos camera
	and mouse_mov_param =
	  mouse_movement lastx lasty xrot yrot
	and scroll_mov_param =
	  scroll_movement xrot yrot xpos ypos zpos camera
	in

	  ignore (Glib.Timeout.add ~ms:20
		    ~callback:
		    (fun () ->
		       let continue = refresh_points uvar t_points in
			 render_param ();
			 continue));

	  let id_display =
	    glArea#connect#display
	      ~callback:(fun () -> render_param ();)
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
	    glArea#drag#connect#motion
	      ~callback:(fun dc ~x ~y ~time ->
			   mouse_mov_param x y;
			   render_param ();false);
	  and id_reniew =
	    glArea#drag#connect#beginning
	      ~callback:(fun dc -> reniew_coord := true)
	  and id_scroll =
	    glArea#event#connect#scroll
	      ~callback:(fun ev ->
			   scroll_mov_param ev;
			   render_param ();false);
	  in

	    ignore (glArea#event#add [`ALL_EVENTS]);

	  (if not !gui_mode then
	     begin
	       !window#show ();
	       GMain.Main.main ();
	     end);

	  (* Return ids to disconnect callbacks later *)
	  [|[id_display;id_reshape;id_mouse;id_scroll;id_reniew];[id_keyboard]|]