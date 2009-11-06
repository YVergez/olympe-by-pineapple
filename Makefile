OCAMLMAKEFILE = OCamlMakefile

SOURCES = gui.mli gui.ml picture_processing.mli picture_processing.ml main.mli main.ml
RESULT  = olympe
INCDIRS = +lablgtk2 +lablGL +sdl
LIBS = bigarray sdl sdlloader str

OCAMLBLDFLAGS = lablgtk.cma  gtkInit.cmo lablgl.cma  lablgtkgl.cma
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx lablgl.cmxa lablgtkgl.cmxa
OCAMLFLAGS = -w s

include $(OCAMLMAKEFILE)
