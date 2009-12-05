OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep
EXEC=olympe

PICTPROCSOURCES = picture_processing.ml
SAMPLINGSOURCES = sampling.ml
DISPLAYSOURCES  = display3D.ml
GUISOURCES      = skel.ml mainview.ml statusbar.ml statebar.ml sidebar.ml dialogs.ml menubar.ml toolbar.ml gui.ml

SRC = $(PICTPROCSOURCES) $(SAMPLINGSOURCES) $(DISPLAYSOURCES) $(GUISOURCES:%=gui/%) main.ml

OBJBYTE= gtkInit.cmo $(SRC:.ml=.cmo)
OBJNAT = gtkInit.cmx $(SRC:.ml=.cmx)
OBJMLI = $(wildcard *.mli) $(wildcard gui/*.mli)
OBJINT = $(OBJMLI:.mli=.cmi)
LIBS= unix bigarray sdl sdlloader str lablgtk lablgl lablgtkgl
INCDIRS= +lablgtk2 +lablGL +sdl +site-lib/sdl gui/
OCAMLFLAGS= -warn-error A
OPTIONS= $(OCAMLFLAGS) $(INCDIRS:%=-I %)

#Compilation rules
all: bc

bc: byte-code

byte-code: $(OBJINT) $(SRC:.ml=.cmo)
	$(OCAMLC) \
	\
	$(OPTIONS) $(LIBS:%=%.cma) -o $(EXEC) 	\
	$(OBJBYTE)

nc: native-code

native-code: $(OBJINT) $(SRC:.ml=.cmx)
	$(OCAMLOPT) \
	\
	$(OPTIONS) $(LIBS:%=%.cmxa) -o $(EXEC) 	\
	$(OBJNAT)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo: %.ml
	$(OCAMLC) -c $(OPTIONS) $<

%.cmi: %.mli
	$(OCAMLC) -c $(OPTIONS) $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(OPTIONS) $<

# Clean up
clean: mrproper
	rm -f $(EXEC) *.tar.bz2

mrproper:
	rm -f $(OBJBYTE) $(OBJNAT) $(SRC:.ml=.cmi) $(SRC:.ml=.o) *~ \#*\#

.PHONY: clean mrproper

#Packing up
package: clean
	tar -cjvC ../ -f olympe0.1_sources.tar.bz2 Olympe/