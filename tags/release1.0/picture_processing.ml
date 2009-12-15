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
(*PIXELS*)
(*Compare les pixels adjacents*)

let closed_pix src i j h w =
  if ((i)<h) && ((j)<w) then
    (Sdlvideo.get_pixel src (i)(j))=(Sdlvideo.get_pixel src (i+1)(j))
  && (Sdlvideo.get_pixel src (i)(j))=(Sdlvideo.get_pixel src (i)(j+1))
  else
    if ((i)<h) && ((j)=w) then
      (Sdlvideo.get_pixel src (i)(j))=(Sdlvideo.get_pixel src (i+1)(j))
    else
      if ((i)=h) && ((j)<w) then
	(Sdlvideo.get_pixel src (i)(j))=(Sdlvideo.get_pixel src (i)(j+1))
      else
	if ((i)=h) && ((j)=w) then
	  true
	else
	  false
(*_________________*)

(*EXTENSION*)
(*Recupere et renvoie l'extension du fichier*)

let ext s =
  let i = ((String.rindex s '.')+1) in
  let j = (String.length s) in
    String.sub s i j

(*Supprime l'extension du fichier et son emplacement*)

let del_ext s =
  let i = String.rindex s '.' in
  let j =
    try
      (String.rindex s '/') + 1
    with Not_found -> 0
  in
    String.sub s j (i - j)
(*_________________*)

(*LIST*)
(*Met la liste d'altitudes en ordre croissant*)

let incr_list list =
  let aux (x,y,z,a) (u,v,w,b) = a-b in
    List.sort aux list

(*Ecrit les differentes couleurs et leurs altitudes correspondantes
dans un fichier texte "colors"*)

let rec_color list filename =
  let list = incr_list list in
  let out_channel = open_out filename in
  let rec add = function
    |[] -> ();
    |(r,g,b,h)::t ->
       output_string out_channel ("(r,g,b):(" ^ (string_of_int r) ^ "," ^
				    (string_of_int g) ^ "," ^
				    (string_of_int b) ^ ") altitude:" ^
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
(*_________________*)


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
              Sdlvideo.put_pixel dst ~x:i ~y:j color;
	      let (r,g,b) = Sdlvideo.get_pixel_color src i j in
		list := rf_cur_list (r,g,b) !list;
          end
	else
	  Sdlvideo.put_pixel_color dst ~x:i ~y:j Sdlvideo.black
      done;
    done;
    for j = 0 to (w - 1) do
      begin
	let c = Sdlvideo.get_pixel src (h-2) j in
	  Sdlvideo.put_pixel dst (h-1) j c;
      end
    done;
    for i = 0 to (h-1) do
      begin
	let c = Sdlvideo.get_pixel src i (w-2) in
	  Sdlvideo.put_pixel dst i (w-1) c;
      end
    done;

    let out_filename =
      if out_file = "NONE" then
	"resources/tmp/" ^ del_ext(file) ^ "-edged.bmp"
      else
	out_file
    in
      Sdlvideo.save_BMP dst out_filename;
      (out_filename, List.length !list, !list)
