(* type pour le header bmp*)
type info_file = {
  mutable offset : int;
  mutable headerSize : int;
  mutable width : int;
  mutable height : int;
  mutable nb_bit : int
}

(*type pour un sommet de triangle*)
type sommet = {
  mutable som : (int*int);
  mutable alt : int;
  mutable col : (int*int*int)
}

(*type triangle*)
type triangle = {
  mutable s1 : sommet;
  mutable s2 : sommet;
  mutable s3 : sommet}

(*Lit un fichier bit a bit*)
let transBM openfile = input_byte openfile

(*Lit un fichier par 2 bit*)
let transW openfile = 256 * (input_byte openfile) + (input_byte openfile)

(*lit les fichier par 4 bit*)
let trans2W openfile = 256 * (256 *(256 *(input_byte openfile) + (input_byte openfile))
			     + (input_byte openfile)) + (input_byte
			     openfile)

(*lit les fichier par 3 bit*)
let trans_24bit openfile = (input_byte openfile,input_byte openfile,input_byte openfile)

(*initialise une matrice de triplet (r,g,b)*)
let tableau x y (r,g,b) = Array.make_matrix x y (r,g,b)

(*imprime un doublet*)
let print2D (x,y) =
  Printf.printf "%d%d \n" x y;;

(*imprime un triplet*)
let print3D (x,y,z) =
  Printf.printf "%d%d%d \n" x y z;;

(*transforme un triplet en une chaine*)
let transt2s (x,y,z) =
  "("^(string_of_int x)^","^
    (string_of_int y)^","^
    (string_of_int z)^")";;

(*tranforme un doublt en une chaine*)
let transd2s (x,y) = (string_of_int x)^" "^(string_of_int y);;

let leclist listetri =
  while (!listetri <> []) do
    let prems = List.hd !listetri in
      print_string (transd2s prems.s1.som);
	print_string " ";
      print_string (transd2s prems.s2.som);
	print_string " ";
      print_string (transd2s prems.s3.som);
	print_newline ();
      listetri := List.tl !listetri;
  done

(*convertit un quadruplet en triplet*)
let qua2tri (r,g,b,a) = (r,g,b);;

(*recupere dernier terme d'un quadruplet*)
let der (r,g,b,a) = a;;

let comp_tri (a1,b1,c1) (a2,b2,c2) =
  if (a1 = a2) && (b1 = b2) && (c1 = c2) then
    true
  else
    false

(*recupere les altitudes d'un sommet a partir d'une liste de quadruplet*)
let recupaltitude sm listequadra =
  let l = ref listequadra in
    while (!l <> []) do
      begin
	let prems = List.hd !l in
	let color = qua2tri prems in
	  begin
	    if (comp_tri color sm.col) then
	      sm.alt <- der prems;
	  end;
	  l := List.tl !l;
      end
    done

let print_tri tr =
  print_string (transd2s tr.s1.som);
  print_string " ";
  print_string (transt2s tr.s1.col);
  print_string " ";
  print_int tr.s1.alt;
  print_string " / ";
  print_string (transd2s tr.s2.som);
  print_string " ";
  print_string (transt2s tr.s2.col);
  print_string " ";
  print_int tr.s2.alt;
  print_string " / ";
  print_string (transd2s tr.s3.som);
  print_string " ";
  print_string (transt2s tr.s3.col);
  print_string " ";
  print_int tr.s3.alt;
  print_newline ()


let build_tri1 lbs cbs dl dc pl pc m =
  {s1 = {som = (lbs, cbs);
	 alt = 0;
	 col = m.(cbs).(lbs)};
   s2 = {som = (lbs + dl, cbs + dc);
	 alt = 0;
	 col = m.(cbs + dc).(lbs + dl)};
   s3 = {som = (lbs + pl, cbs + pc);
	 alt = 0;
	 col = m.(cbs + pc).(lbs + pl)}
  }

let build_tri2 lbs cbs dl dc pl pc m =
  {s1 = {som = (lbs, cbs);
	 alt = 0;
	 col = m.(cbs).(lbs)};
   s2 = {som = (lbs + pl, cbs + pc);
	 alt = 0;
	 col = m.(cbs + pc).(lbs + pl)};
   s3 = {som = (lbs + dl, cbs + dc);
	 alt = 0;
	 col = m.(cbs + dc).(lbs + dl)}
  }

(*recupere les coordonnées des sommets des triangles,leurs altitudes et
leurs couleurs*)
let recuptriangles m pas lg cl listequadra =
  let listri = ref [] in
  let i = ref 0 in
    while (!i < (lg - pas)) do
      begin
	let j = ref 0 in
	  while (!j < (cl - pas)) do
	    begin
		(*recuperation de AEB*)
	      let ref_tr =
		build_tri2 !i !j 0 pas (pas/2) (pas/2) m in
		  (*affectation des altitudes en fonctions des couleurs*)
		recupaltitude ref_tr.s1 listequadra;
		recupaltitude ref_tr.s2 listequadra;
		recupaltitude ref_tr.s3 listequadra;
		  (*impression triangle*)
		(*print_tri ref_tr;*)
		  (*insertion dans la liste*)
		listri := ref_tr::!listri;
		  (*recuperation de ADE*)
		let ref_tr =
		  build_tri1 !i !j pas 0 (pas/2) (pas/2) m in
		    (*affectation des altitudes en fonctions des couleurs*)
		  recupaltitude ref_tr.s1 listequadra;
		  recupaltitude ref_tr.s2 listequadra;
		  recupaltitude ref_tr.s3 listequadra;
		    (*impression triangle*)
		  (*print_tri ref_tr;*)
	            (*insertion dans la liste*)
		  listri := ref_tr::!listri;
	            (*recuperation BEC*)
		  let ref_tr =
		    build_tri2 !i (!j + pas) pas 0 (pas/2) (-pas/2) m in
	              (*affectation des altitudes en fonctions des couleurs*)
		    recupaltitude ref_tr.s1 listequadra;
		    recupaltitude ref_tr.s2 listequadra;
		    recupaltitude ref_tr.s3 listequadra;
		      (*impression triangle*)
		    (*print_tri ref_tr;*)
	              (*insertion dans liste*)
		    listri := ref_tr::!listri;
	              (*recuperation DCE*)
		    let ref_tr =
		      build_tri1 (!i + pas) !j  0 pas (-pas/2) (pas/2) m in
	                (*affectation des altitudes en fonctions des couleurs*)
		      recupaltitude ref_tr.s1 listequadra;
		      recupaltitude ref_tr.s2 listequadra;
		      recupaltitude ref_tr.s3 listequadra;
		        (*impression triangle*)
		      (*print_tri ref_tr;*)
	                (*insertion dan sla liste*)
		      listri := ref_tr::!listri;
		      j := !j + pas;
	    end;
	  done;
	    i := !i + pas;
      end;
    done;
    !listri;;

(*on ecrit dans un fichier obj les sommets puis les faces des triangles trouvés*)
let writeobj filename listriangle=
  let file = open_out filename in
  let i = ref 1 in
    while (!listriangle <> [] ) do
      let tria = List.hd !listriangle in
	output_string file ("v "^ (transd2s tria.s1.som)^
			      " "^(string_of_int (tria.s1.alt))^"\n");
	output_string file ("v "^ (transd2s tria.s2.som)^
			      " "^(string_of_int (tria.s2.alt))^"\n");
	output_string file ("v "^ (transd2s tria.s3.som)^
			      " "^(string_of_int (tria.s3.alt))^"\n");
	listriangle := (List.tl !listriangle);
	i := !i + 3;
    done;
    let j = ref 1 in
      while(!j < !i) do
	output_string file ("f "
			    ^(string_of_int (!j))^" "
			    ^(string_of_int (!j + 1))^" "
			    ^(string_of_int (!j + 2))^"\n");
	j := !j + 3;
      done;
      close_out file;;

let openbmp filename out_filename listequadra pas =
  let file = open_in filename in
  let i = ref 0 in
  let t_ref = {offset = 0;headerSize = 0;width = 0;height = 0;nb_bit = 0} in
    (try
(*       print_newline();*)
       begin
	 while (!i <= 28) do
	   if (!i < 2) then
	   (*Recuperation de BM premier octet d'un BMP*)
	   let s = transBM file in
	     print_int s;
	     print_newline ();
	     i := !i + 1;
	   else
	     begin
	     (*recuperation du nb de plan et nb bit par pixel*)
	       if (!i = 26) || (!i = 28) then
		 let s = transW file in
		   (*	     print_int s;*)
		   if (!i = 28) then
		     begin
		       t_ref.nb_bit <- s;
(*		       print_newline ();
		       print_string "t_ref.nb_bit = ";
		       print_int t_ref.nb_bit;*)
		     end;
		   i := !i + 2;
	       else
	       (*recuperation des octet par 4*)
		 let s = trans2W file in
		   if (!i = 10) then
		     begin
		       t_ref.offset <- s;
(*		       print_string "t_ref.offset = ";
		       print_int t_ref.offset;
		       print_newline();*)
		     end;
		   if (!i = 14) then
		     begin
		       t_ref.headerSize <- s;
(*		       print_string "t_ref.HeaderSize = ";
		       print_int t_ref.headerSize;
		       print_newline();*)
		     end;
		   if (!i = 18) then
		     begin
		       t_ref.width <- s;
(*		       print_string "t_ref.width = ";
		       print_int t_ref.width;
		       print_newline ();*)
		     end;
		   if (!i = 22) then
		     begin
		       t_ref.height <- s;
(*		       print_string "t_ref.height = ";
		       print_int t_ref.height;
		       print_newline ();*)
		     end;
(*		 print_int s;*)
		 i := !i + 4;
	     end;
(*	   print_char ' ';
	   print_newline();*)
	 done;
     end;
       i := t_ref.offset;
(*       print_int !i;
       print_newline();
       print_int (pos_in file);
       print_newline();*)
       seek_in file !i;
(*       print_int (pos_in file);*)
       let m = Array.make_matrix t_ref.width t_ref.height (0,0,0)in
	 for l = 0 to t_ref.height - 1 do
	   let ml = m.(l) in
	     for c = 0 to t_ref.width - 1 do
	       ml.(c) <- trans_24bit file;
 	     done;
	 done;
(* la liste quadra que guillaume et yohan doivent passer en
parametre*)
(* je t'en ai fait une pour tester*)
(*	 let listequadra = [(0,0,255,10);(0,255,0,20);(255,0,0,30)] in*)
	 let listriangle =
	   ref (recuptriangles m pas (t_ref.height) (t_ref.width) listequadra) in
	   writeobj out_filename listriangle;
     with End_of_file -> close_in file)


(*let main () =
  begin
    let listequadra = [(0,0,255,1220);(0,255,0,2220);(255,0,0,3220)] in
      openbmp Sys.argv.(1) listequadra (int_of_string Sys.argv.(2));
    exit 0;
  end

let _ = main ();;
*)

(* Added by Guillaume:
val open_bmp : (filename:string) (out_filnename:string) (colorsAndHeight:(int * int* int * int) list) (pas:int) = <fun>
Create the .obj of <filename> in <out_filename>.
*)
