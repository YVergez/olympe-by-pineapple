let main () =
  let img      = ref "" 
  and pre_img  = ref "resources/tmp/pre_img.bmp"
  and inter    = ref 10
  and obj_file = ref "resources/tmp/final_obj.obj"
  and gui_mode = ref false
  and usage = "Description :
     Olympe transform a topological map in its 3D representation.\n
Usage : olympe [OPTION...] IMAGE_FILE\n" in 

    Arg.parse
      (Arg.align [
	 ("-p", Arg.Set_string(pre_img), " save the post-treated image file.");
	 ("-i", Arg.Set_int(inter), " set the interval for image sampling.");
	 ("-o", Arg.Set_string(obj_file), " save the final 3D model (.obj)");
	 ("-g", Arg.Set(gui_mode), " launch Olympe's GUI");
	 ("--gui", Arg.Set(gui_mode), " launch Olympe's GUI")])
      (fun img_name -> img := img_name) usage;

    if !gui_mode then
      begin
	Gui.init ();
	GMain.Main.main ();
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
	        (*pre_treat !img !pre_img;
	        sample !pre_img !inter !obj_file;
	        draw_3d !obj_file*)
	      end
	  end
      end
	

let _ = main ()
