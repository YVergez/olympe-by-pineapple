OCAMLMAKEFILE = OCamlMakefile

PICTPROCSOURCES = picture_processing.mli picture_processing.ml
SAMPLINGSOURCES = sampling.mli sampling.ml
DISPLAYSOURCES  = display3D.mli display3D.ml
GUISOURCES      = gui/skel.mli gui/skel.ml gui/mainview.mli gui/mainview.ml gui/statusbar.mli gui/statusbar.ml gui/sidebar.mli gui/sidebar.ml gui/dialogs.mli gui/dialogs.ml gui/menubar.mli gui/menubar.ml gui/toolbar.mli gui/toolbar.ml gui/statebar.mli gui/statebar.ml gui/gui.mli gui/gui.ml

SOURCES = $(PICTPROCSOURCES) $(SAMPLINGSOURCES) $(DISPLAYSOURCES) $(GUISOURCES) main.mli main.ml
RESULT  = olympe
# site-lib/sdl dir is for PIE compilation
INCDIRS = +lablgtk2 +lablGL +sdl +site-lib/sdl
LIBS = bigarray sdl sdlloader str lablglut

OCAMLBLDFLAGS = lablgtk.cma  gtkInit.cmo lablgl.cma  lablgtkgl.cma
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx lablgl.cmxa lablgtkgl.cmxa
(*OCAMLFLAGS = -warn-error A*)

include $(OCAMLMAKEFILE)