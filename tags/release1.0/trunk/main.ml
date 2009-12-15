let clean_tmp () =
  let filenames = Sys.readdir "resources/tmp/" in
    for i = 0 to (Array.length filenames) - 1 do
      if filenames.(i).[0] <> '.' then
	Sys.remove ("resources/tmp/" ^ filenames.(i))
    done

let ask_heights auto colors =
  let rec heights_rec i = function
      [] -> []
    | (r,g,b)::t ->
	begin
	  Printf.printf "Witch altitude is represented by color (%d,%d,%d) (r,g,b) ? " r g b;
	  flush stdout;
	  if auto then
	    begin
	      Printf.printf "%i\n" i;
	      flush stdout;
	      (r,g,b,i)::(heights_rec (i + 10) t)
	    end
	  else
	    begin
	      let add h =
		(r,g,b,h)::(heights_rec i t) in
		Scanf.bscanf Scanf.Scanning.stdib "%d\n" add;
	    end
	end
  in
    heights_rec 0 colors

let main () =
  let img      = ref ""
  and pre_img  = ref "NONE"
  and inter    = ref 10
  and obj_file = ref "resources/tmp/final_obj.obj"
  and gui_mode = ref false
  and auto     = ref false
  and usage = "Description :
     Olympe transform a topological map in its 3D representation.\n
Usage : olympe [OPTION...] IMAGE_FILE\n" in

    Arg.parse
      (Arg.align [
	 ("-p", Arg.Set_string(pre_img), " save the post-treated image file.");
	 ("-i", Arg.Set_int(inter), " set the interval for image sampling.");
	 ("-a", Arg.Set(auto), " auto-fill the altitudes. (not recommended)");
	 ("-o", Arg.Set_string(obj_file), " save the final 3D model (.obj)");
	 ("-g", Arg.Set(gui_mode), " launch Olympe's GUI");
	 ("--gui", Arg.Set(gui_mode), " launch Olympe's GUI")])
      (fun img_name -> img := img_name) usage;

    if !gui_mode then
      begin
	Gui.init ();
	clean_tmp ();
      end
    else
      begin
	if !img = "" then
	  failwith "No file specified. You must specify an image file to
  treat."
	else
	  begin
	    if not (Sys.file_exists !img) then
	      failwith "The file you specified do not exists."
	    else
	      begin
		print_endline "Picture processing...\n";
	        let (out_file, alt_nb, colors) =
		  Picture_processing.process_img !img !pre_img in
		  print_endline "Asking colors...\n";
		  let cAndH = ask_heights !auto colors in
		    print_endline "Sampling...\n";
	            Sampling.do_all !img out_file !obj_file !inter cAndH;
		    print_endline "Display...\n";
	            let _ =
		      Display3D.draw_map "-f" ~colors:cAndH !obj_file in
		      clean_tmp ();
	      end
	  end
      end


let _ = main ()
