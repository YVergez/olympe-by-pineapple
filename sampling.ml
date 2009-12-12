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

(*transforme un triplet en une chaine*)
let transt2s (x,y,z) =
  "("^(string_of_int x)^","^
    (string_of_int y)^","^
    (string_of_int z)^")";;

(*tranforme un doublon en une chaine*)
let transd2s (x,y) = (string_of_int x)^" "^(string_of_int y)

(*prend le premier elt d'un doublon*)
let premsD (x,y) = x

(*prend le dernier elt d'un doublon*)
let derD (x,y) = y

(*convertit un quadruplet en triplet*)
let qua2tri (r,g,b,a) = (r,g,b)

(*recupere dernier terme d'un quadruplet*)
let der (r,g,b,a) = a

(*egalité de deux triplets*)
let comp_tri (a1,b1,c1) (a2,b2,c2) =
  if (a1 = a2) && (b1 = b2) && (c1 = c2) then
    true
  else
    false

(*recupere les altitudes d'un sommet a partir d'une liste de quadruplet*)
let recupaltitude (x,y) msl listequadra =
  let l = ref listequadra in
  let cds = ref false in
  let alt = ref 0 in
    while (!l <> []) && not(!cds) do
      begin
	let prems = List.hd !l in
	let color = qua2tri prems in
          (*on compare la couleur de  la liste et l couleur du
sommet*)
	  if (comp_tri color msl.(x).(y)) then
	    begin
	      alt := der prems;
	    (*on a trouvé la couleur,on va s'arreter*)
	      cds := true;
	    end;
	  l := List.tl !l;
      end
    done;
    !alt

(*construit un sommet*)
let build_sm sommet alti color =
  {som = sommet;
   alt = alti;
   col = color}

(*premiere maniere de derterminer un triangle dans un carré*)
let build_tri1 lbs cbs dl dc pl pc m l_alt =
  {s1 = {som = (lbs, cbs);
	 alt = recupaltitude (lbs,cbs) m l_alt;
	 col = m.(cbs).(lbs)};
   s2 = {som = (lbs + dl, cbs + dc);
	 alt = recupaltitude (lbs + dl, cbs + dc) m l_alt;
	 col = m.(cbs + dc).(lbs + dl)};
   s3 = {som = (lbs + pl, cbs + pc);
	 alt = recupaltitude (lbs + pl, cbs + pc) m l_alt;
	 col = m.(cbs + pc).(lbs + pl)}
  }

(*deuxieme maniere de determiner un triangle dans un carré*)
let build_tri2 lbs cbs dl dc pl pc m l_alt =
  {s1 = {som = (lbs, cbs);
	 alt = recupaltitude (lbs,cbs) m l_alt;
	 col = m.(cbs).(lbs)};
   s2 = {som = (lbs + pl, cbs + pc);
	 alt = recupaltitude (lbs + pl, cbs + pc) m l_alt;
	 col = m.(cbs + pc).(lbs + pl)};
   s3 = {som = (lbs + dl, cbs + dc);
	 alt = recupaltitude (lbs + dl, cbs + dc) m l_alt;
	 col = m.(cbs + dc).(lbs + dl)}
  }

(*autre maniere de construire un triangle*)
let build_tri3 som1 som2 som3 m =
  {s1 = som1;
   s2 = som2;
   s3 = som3
  }

(*Creation d'une liste avec les intersections entre les lignes des
triangles et celle des lignes de niveau entre deux sommets de grand triangle*)
let internl sma smb msl mal list_alt=
  let x1 = ref (premsD(sma.som)) in
  let y1 = ref (derD(sma.som)) in
  let x2 = ref (premsD(smb.som)) in
  let y2 = ref (derD(smb.som)) in
  let listeAB = ref [sma] in
  let dx = !x2 - !x1 in
  let dy = !y2 - !y1 in
  let incx = ref 0 in
  let incy = ref 0 in
    if dx > 0 then
      incx := 1;
    if dx < 0 then
      incx := -1;
    if dy > 0 then
      incy := 1;
    if dy < 0 then
      incy := -1;
    let x = ref !x1 in
    let y = ref !y1 in
    while ((!x <> !x2) && (!y <> !y2)) do (*ligne que l'on va etudier*)
      (*On regarde la couleur du pixel dans la matrice avec ligne*)
      (*si c'est noir on met va choper sa couleur dans la matrice sans
       ligne*)
      if (mal.(!x).(!y) = (0,0,0)) then
	begin
	  (*on va recuperer l'altitude du sommet*)
	  let alt = recupaltitude (!x,!y) msl list_alt in
          (*creation du sommet intermediaire*)
	  let newsm = build_sm (!x,!y) alt msl.(!x).(!y) in
	    (*on insère le sommet dans la liste*)
	    listeAB := newsm::!listeAB;
 	end;
      (*sinon on continue*)
      x := !x + !incx;
      y := !y + !incy;
    done;
    listeAB := !listeAB@[smb];
    !listeAB

(*union de 2 liste avec doublon eliminé*)
let merge lab lbc = lab@(List.tl lbc)

(*fonction de creation des triangles avec sommet des intersections d
eligne de niveau*)
let create_subtri soma somb somc msl mal list_alt =
  let listeAB = ref (internl soma somb msl mal list_alt) in
  let listeBC = ref (internl somb somc msl mal list_alt) in
  let listeAC = ref (internl soma somc msl mal list_alt) in
  let liste_AC = ref (merge !listeAB !listeBC) in
  let prec_AC = ref (List.hd !liste_AC) in
  let precAC = ref (List.hd !listeAC) in
    liste_AC := List.tl !liste_AC;
    listeAC := List.tl !listeAC;
    let suiv_AC = ref (List.hd !liste_AC) in
    let suivAC = ref (List.hd !listeAC) in
    let result_list = ref [] in
    let cda = ref true in
      while (List.length !listeAC > 2) || (List.length !liste_AC > 2)
      do
	if ((!prec_AC = somb) && (List.length !listeAC = 1)) then
	  cda := not(!cda);
	if (List.length !liste_AC > 2) && !cda  then
	  let ref_tr = build_tri3 !precAC !suiv_AC !suivAC msl in
	    result_list := ref_tr::!result_list;
	    liste_AC := List.tl !liste_AC;
	    prec_AC := !suiv_AC;
	    suiv_AC := List.hd !liste_AC;
	    let ref_tr = build_tri3 !suivAC !suiv_AC !prec_AC msl in
	      result_list := ref_tr::!result_list;
	else
	  begin
	    let ref_tr = build_tri3 !precAC !prec_AC !suivAC msl in
	      result_list := ref_tr::!result_list;
	  end;
	if (List.length !listeAC > 2) then
	  begin
	    listeAC := List.tl !listeAC;
	    precAC := !suivAC;
	    suivAC := List.hd !listeAC;
	  end;
      done;
      !result_list

(*On va recuperer les sommets de toutes les intersections*)
(*recuperer les coord des sommets des triangles,alt & color*)
let recuptriangles msl mal pas lg cl l_alt =
  let listri = ref [] in
  let i = ref 0 in
    while (!i < (lg - pas)) do
      begin
	let j = ref 0 in
	  while (!j < (cl - pas)) do
	    begin
		(*recuperation de AEB*)
	      let ref_tr =
		build_tri2 !i !j 0 pas (pas/2) (pas/2) msl l_alt in
		(*recuperation des listes de points d'intersec entre
sommet*)
		let liste_inter =
		  create_subtri ref_tr.s1 ref_tr.s2 ref_tr.s3 msl mal
		    l_alt in
		  (*insertion dans la liste*)
		  if liste_inter = [] then
		    listri := ref_tr::!listri
		  else
		    listri := liste_inter@(!listri);
		  (*recuperation de ADE*)
		  let ref_tr =
		    build_tri1 !i !j pas 0 (pas/2) (pas/2) msl l_alt in
		    (*listes des petits triangle*)
		    let liste_inter =
		      create_subtri ref_tr.s1 ref_tr.s3 ref_tr.s2 msl
			mal l_alt in
		      (*insertion dans la liste*)
		      if liste_inter = [] then
			listri := ref_tr::!listri
		      else
			listri := liste_inter@(!listri);
	             (*recuperation BEC*)
		      let ref_tr =
			build_tri2 !i (!j + pas) pas 0 (pas/2)
			  (-pas/2) msl l_alt in
			let liste_inter =
			  create_subtri ref_tr.s1 ref_tr.s2 ref_tr.s3
			    msl mal l_alt in
		          (*insertion dans la liste*)
			  if liste_inter = [] then
			    listri := ref_tr::!listri
			  else
			    listri := liste_inter@(!listri);
	                 (*recuperation DCE*)
			  let ref_tr =
			    build_tri1 (!i + pas) !j  0 pas (-pas/2) (pas/2)
			      msl l_alt in
			  let liste_inter =
			    create_subtri ref_tr.s1 ref_tr.s3
			      ref_tr.s2 msl mal l_alt in
		              (*insertion dans la liste*)
			    if liste_inter = [] then
			      listri := ref_tr::!listri
			    else
			      listri := liste_inter@(!listri);
			    j := !j + pas;
	    end;
	  done;
	  i := !i + pas;
      end;
    done;
    !listri

(*on ecrit dans le point obj*)
let writeobj filename list_tri =
  let i = ref 1 in
  let j = ref 1 in
  let file = open_out filename in
    while (!list_tri <> [] ) do
      let triangle = List.hd !list_tri in
	output_string file ("v "^(transd2s triangle.s1.som)^
			      " "^(string_of_int (triangle.s1.alt))^"\n"^
			      "v "^(transd2s triangle.s2.som)^
			      " "^(string_of_int (triangle.s2.alt))^"\n"^
			      "v "^(transd2s triangle.s3.som)^
			      " "^(string_of_int (triangle.s3.alt))^"\n");

	i := !i + 3;
	list_tri := (List.tl !list_tri);
    done;
    while(!j <> !i) do
      output_string file ("f " ^(string_of_int (!j)^" "
				 ^(string_of_int (!j + 1))^" "
				 ^(string_of_int (!j + 2))^"\n"));
	j := !j + 3;
    done;
    close_out file

(*initialise une matrice*)
let init_matrix longueur largeur =
  Array.make_matrix longueur largeur (0,0,0)

(*fonction lecture de l'entete d'un bmp*)
let openbmp filename t_ref =
  let file = open_in filename in
  let i = ref 0 in
    (try
       while (!i <= 28) do
	 if (!i < 2) then
	   (*Recuperation de BM premier octet d'un BMP*)
	   let s = transBM file in
	     i := !i + 1;
	 else
	   (*recuperation du nb de plan et nb bit par pixel*)
	   if (!i = 26) || (!i = 28) then
	     let s = transW file in
	       if (!i = 28) then
		 t_ref.nb_bit <- s;
	       i := !i + 2;
	   else
	     (*recuperation des octets par 4*)
	     let s = trans2W file in
	       if (!i = 10)   then
		 begin
		   t_ref.offset <- s;
		 end;
	       if (!i = 14) then
		 begin
		   t_ref.headerSize <- s;
		 end;
	       if (!i = 18) then
		 begin
		   t_ref.width <- s;
		 end;
	       if (!i = 22) then
		 begin
		   t_ref.height <- s;
		 end;
	       i := !i + 4;
       done;
     with End_of_file -> close_in file);
    t_ref

(*on creer la matrice*)
let make_matrice t_ref filename =
  let file = open_in filename in
  let t_ref = openbmp filename t_ref in
  let m = (init_matrix t_ref.width t_ref.height) in
    seek_in file (t_ref.offset);
       (* fabrication matrice à partir carte origine*)
    (try
       for l = 0 to t_ref.height - 1 do
	 let ml = (m.(l)) in
	   for c = 0 to t_ref.width - 1 do
	     ml.(c) <- trans_24bit file;
 	   done;
       done;
     with End_of_file -> close_in file);
    m

(*fonction pricipale*)
let do_all carteori cartelig obj_filename pas listequadra =
  let t_ref =
    {offset = 0;headerSize = 0;width = 0;height = 0;nb_bit = 0} in
  (*matrice carte origine*)
  let mori = make_matrice t_ref carteori in
  (*matrice carte ligne*)
  let mlig = make_matrice t_ref cartelig in
  (* la liste quadra que guillaume et yohan doivent passer en
     parametre*)
  (* je t'en ai fait une pour tester*)
    let listriangle =
      ref (recuptriangles mori mlig pas (t_ref.height) (t_ref.width)
	     listequadra) in
      writeobj obj_filename listriangle;;
