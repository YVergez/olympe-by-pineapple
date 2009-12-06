(*
*******************************************************
* Nom .......... : picture_processing.ml
* Role ......... : Trace les limites separant chaque
*                  zone de couleur et renvoi un fichier
*                  contenant les differentes altitudes
* Auteur ....... : Vergez Yohann
* Version ...... : 1.0
* License ...... : GPL v3

*******************************************************
*)

(*Compare les pixels adjacents*)

let closed_pix src i j h w =
  if ((i+1)<h) && ((j+1)<w) then
   (Sdlvideo.get_pixel src (i)(j))=(Sdlvideo.get_pixel src (i+1)(j))
   && (Sdlvideo.get_pixel src (i)(j))=(Sdlvideo.get_pixel src (i)(j-1))
  else
    false


(*Cree la matrice de convolution 3/3 correspondante
aux 8 pixels entourants le pixel courant.
Pixel en dehors de l'image : valeur du pixel central.*)
(*
let create_matrix src i j h w =
  let mat = Array.make 9 0 in
  let cpt = ref 0 in
    for dj = (-1) to 1 do
      for di = (-1) to 1 do
        if (((i+di)>=0)&&((i+di)<h)&&((j+dj)>=0)&&((j+dj)<w)) then
          mat.(!cpt) <-Int32.to_int(Sdlvideo.get_pixel src (i+di)(j+dj))
        else
          mat.(!cpt) <-Int32.to_int(Sdlvideo.get_pixel src i j);
        cpt := !cpt+1;
      done;
    done;
    mat
*)
(*Applique le filtre de Sobel au pixel courant.*)
(*
let filter src i j h w =
  let mat = create_matrix src i j h w in
  let filterx = (-1)*mat.(0) + 1*mat.(2) + (-2)*mat.(3)
    + 2*mat.(5) + (-1)*mat.(6) + 1*mat.(8)
  and
      filtery = 1*mat.(0) +2*mat.(1) +1*mat.(2)
    + (-1)*mat.(6) + (-2)*mat.(7) + (-1)*mat.(8) in
  let res = (abs(filterx) + abs(filtery)) / 8 in
    res
*)
(*Supprime l'extension du fichier et son emplacement*)

let del_ext s =
  let i = String.rindex s '.' in
  let j =
    try
      (String.rindex s '/') + 1
    with Not_found -> 0
  in
    String.sub s j (i - j)

(*Met la liste d'altitudes en ordre croissant*)

let incr_list list =
  let aux (x,y,z,a) (u,v,w,b) = a-b in
    List.sort aux list
(*
  let rec put_in_place = function
      [] -> []
    |e::[] -> e::[]
    |(r1,g1,b1,h1)::(r2,g2,b2,h2)::l ->
       if h1 > h2 then
	 (r2,g2,b2,h2)::(put_in_place ((r1,g1,b1,h1)::l))
       else
	 (r1,g1,b1,h1)::(put_in_place ((r2,g2,b2,h2)::l))
  in
  let r_list = ref list in
    for i = 0 to ((List.length list) - 1) do
      r_list := put_in_place !r_list;
    done;
<<<<<<< .mine
    !r_list
=======
    list[]
>>>>>>> .r30*)



(*Ecrit les differentes couleurs et leurs altitudes correspondantes
dans un fichier texte "colors"*)

let rec_color list filename =
  let out_channel = open_out filename in
  let rec add = function
    |[] -> ();
    |(r,g,b,h)::t ->
       output_string out_channel ((string_of_int r) ^ " " ^
				    (string_of_int g) ^ " " ^
				    (string_of_int b) ^ " " ^
				    (string_of_int h) ^ "\n");
	add t;
  in add list;
    close_out out_channel

(*Met a jour la liste des differentes couleurs*)

let rec rf_cur_list triplet = function
  |[] -> triplet ::[]
  |e::l ->
     if e <> triplet then
       (e::(rf_cur_list triplet l))
     else
       (e::l)

(*Main*)

let process_img file out_file =
  let src = Sdlloader.load_image file in
  let (w,h,pitch) = Sdlvideo.surface_dims src in
  let list = ref [] in
  let dst = Sdlvideo.create_RGB_surface_format src [] w h in
    Sdlvideo.lock src;
    Sdlvideo.lock dst;
    for j = 0 to (w-1) do
      for i = 0 to (h-1) do
        if (closed_pix src i j h w) then
          begin
	    let color = Sdlvideo.get_pixel src i j in
              Sdlvideo.put_pixel dst i j color;
	      let (r,g,b) = Sdlvideo.get_pixel_color src i j in
		list := rf_cur_list (r,g,b) !list;
          end
	else
	  Sdlvideo.put_pixel_color dst i j Sdlvideo.black
      done;
    done;

    let out_filename =
      if out_file = "NONE" then
	"resources/tmp/" ^ del_ext(file) ^ "-edged.bmp"
      else
	out_file
    in
      Sdlvideo.save_BMP dst out_filename;
      (out_filename, List.length !list, !list)
