let statebar_button1 = ref (GButton.button ())
and statebar_button2 = ref (GButton.button ())
and statebar_button3 = ref (GButton.button ())

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

    statebar_button1 := button1;
    statebar_button2 := button2;
    statebar_button3 := button3

(* Used to get a pointer on one of the 3 state buttons. Useful for deactivating button *)
let get = function
    0 -> !statebar_button1
  | 1 -> !statebar_button2
  | 2 -> !statebar_button3
  | n -> failwith ("Invalid call to Statebar.get : " ^ (string_of_int n) ^ "th element do not exist.")
