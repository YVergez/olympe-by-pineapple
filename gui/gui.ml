(* --- GUI INIT --- *)
let init () =
  (* Create the main window *)
  Skel.windowCreate ();
  Menubar.create ();
  Toolbar.create ();

  (* Show the window and enter program's main loop *)
  (Skel.windowGet ())#show ();
  GMain.Main.main ()
