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
INCDIRS= +lablgtk2 +lablGL +sdl gui/
OCAMLFLAGS= -warn-error A
OPTIONS= $(INCDIRS:%=-I %)

#Compilation rules
all: bc

bc: byte-code

byte-code: $(OBJINT) $(SRC:.ml=.cmo)
	$(OCAMLC) \
	\
	$(OCAMLFLAGS) $(OPTIONS) $(LIBS:%=%.cma) -o $(EXEC)     \
	$(OBJBYTE)

nc: native-code

native-code: $(OBJINT) $(SRC:.ml=.cmx)
	$(OCAMLOPT) \
	\
	$(OCAMLFLAGS) $(OPTIONS) $(LIBS:%=%.cmxa) -o $(EXEC)    \
	$(OBJNAT)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $(OCAMLFLAGS) $(OPTIONS) $<

.mli.cmi:
	$(OCAMLC) -c $(OCAMLFLAGS) $(OPTIONS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(OCAMLFLAGS) $(OPTIONS) $<

# Clean up
clean:
	rm -f $(OBJBYTE) $(OBJNAT) $(SRC:.ml=.cmi) $(SRC:.ml=.o) *~ \#*\# gui/*~ gui/\#*\#

mrproper: clean
	rm -f $(EXEC) *.tar.bz2 .depend

.PHONY: clean mrproper

#Packing up
package: mrproper
	tar -cjvC ../ -f olympe_sources.tar.bz2 Olympe/

#Dependencies
.depend:
	ocamldep -I gui/ $(OBJMLI) $(SRC) > .depend

-include .depend
