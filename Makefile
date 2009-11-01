OCAMLMAKEFILE = OCamlMakefile

SOURCES = gui.mli gui.ml main.ml
RESULT  = olympe
INCDIRS = +lablgtk2 +lablgl

OCAMLBLDFLAGS = lablgtk.cma  gtkInit.cmo lablgl.cma  lablgtkgl.cma
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx lablgl.cmxa lablgtkgl.cmxa
OCAMLFLAGS = -w s

include $(OCAMLMAKEFILE)
