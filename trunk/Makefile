COMPIL_BYTE=ocamlc -w s
COMPIL_NATI=ocamlopt -w s


byte:
	$(COMPIL_BYTE) -o olympe -I +lablgtk2 lablgtk.cma gtkInit.cmo gui.ml

opt:
	$(COMPIL_NATI) -o olympe -I +lablgtk2 lablgtk.cmxa gtkInit.cmx gui.ml