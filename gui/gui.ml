(* --- GUI INIT --- *)
let init () =
  (* Create the main components of the window *)
  Skel.windowCreate ();
  Menubar.create ();
  Toolbar.create ();
  Statebar.create ();
  Sidebar.create ();
  Mainview.init ();
  Statusbar.create ();
  Toolbar.initFileDnD ();

  (* Show the window and enter program's main loop *)
  (Skel.getWindow ())#show ();
  GMain.Main.main ()
