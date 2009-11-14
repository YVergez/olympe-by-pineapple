OCAMLMAKEFILE = OCamlMakefile

SOURCES = picture_processing.mli picture_processing.ml sampling.mli sampling.ml display3D.mli display3D.ml gui.mli gui.ml main.mli main.ml
RESULT  = olympe
INCDIRS = +lablgtk2 +lablGL +sdl +site-lib/sdl
LIBS = bigarray sdl sdlloader str lablglut

OCAMLBLDFLAGS = lablgtk.cma  gtkInit.cmo lablgl.cma  lablgtkgl.cma
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx lablgl.cmxa lablgtkgl.cmxa
OCAMLFLAGS =

include $(OCAMLMAKEFILE)