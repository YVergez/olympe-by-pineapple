(* --- GUI INIT --- *)
let init () =
  (* Create the main window *)
  Skel.windowCreate ();
  Menubar.create ();
  Toolbar.create ();
  Statebar.create ();
  Sidebar.create ();
  Mainview.init ();
  Statusbar.create ();

  (* Show the window and enter program's main loop *)
  (Skel.getWindow ())#show ();
  GMain.Main.main ()
