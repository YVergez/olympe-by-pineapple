let statebar_button1 = ref (GButton.button ())
and statebar_button2 = ref (GButton.button ())
and statebar_button3 = ref (GButton.button ())

(* Used to get a pointer on one of the 3 state buttons. Useful for deactivating button *)
let get = function
    0 -> !statebar_button1
  | 1 -> !statebar_button2
  | 2 -> !statebar_button3
  | n -> failwith ("Invalid call to Statebar.get : " ^ (string_of_int n) ^ "th element do not exist.")

let moveToState n () =
  match n with
      0 ->
	Mainview.showMainMap ();
	Skel.showOnlyChild 0 Skel.toolbar_vbox;
	Skel.showOnlyChild 0 Skel.sidebar_vbox;
    | 1 ->
	!statebar_button1#misc#set_sensitive true;
	!statebar_button2#misc#set_sensitive true;
	Mainview.showMainMapEdged ();
	Skel.showOnlyChild 1 Skel.toolbar_vbox;
	Skel.showOnlyChild 1 Skel.sidebar_vbox;
    | 2 ->
	!statebar_button3#misc#set_sensitive true;
	Mainview.showMain3DView ();
	Skel.showOnlyChild 2 Skel.toolbar_vbox;
	Skel.showOnlyChild 2 Skel.sidebar_vbox;
    | _ -> ()

(* Create the statebar *)
let create () =
  let button1 = GButton.button
    ~label:"1 Pre-treatment"
    ~packing:Skel.statebar_hbox#add ()
  and button2 = GButton.button
    ~label:"2. Sampling"
    ~packing:Skel.statebar_hbox#add ()
  and button3 = GButton.button
    ~label:"3. 3D view"
    ~packing:Skel.statebar_hbox#add () in

    button1#misc#set_sensitive false;
    button2#misc#set_sensitive false;
    button3#misc#set_sensitive false;

    ignore (button1#connect#clicked ~callback:(moveToState 0));
    ignore (button2#connect#clicked ~callback:(moveToState 1));
    ignore (button3#connect#clicked ~callback:(moveToState 2));

    statebar_button1 := button1;
    statebar_button2 := button2;
    statebar_button3 := button3
