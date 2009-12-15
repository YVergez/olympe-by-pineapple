OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep
EXEC=olympe

PICTPROCSOURCES = picture_processing.ml
SAMPLINGSOURCES = sampling.ml
DISPLAYSOURCES  = display3D.ml
GUISOURCES      = skel.ml help.ml mainview.ml statusbar.ml statebar.ml \
		  sidebar.ml dialogs.ml toolbar.ml menubar.ml gui.ml

SRC = $(PICTPROCSOURCES) $(SAMPLINGSOURCES) $(DISPLAYSOURCES) $(GUISOURCES:%=gui/%) main.ml

OBJBYTE= gtkInit.cmo $(SRC:.ml=.cmo)
OBJNAT = gtkInit.cmx $(SRC:.ml=.cmx)
OBJMLI = $(wildcard *.mli) $(wildcard gui/*.mli)
OBJINT = $(SRC:.ml=.cmi)
LIBS= unix bigarray sdl sdlloader str lablgtk lablgl lablgtkgl xml-light
INCDIRS= +lablgtk2 +lablGL +sdl +xml-light gui/
OCAMLFLAGS= -warn-error A
OPTIONS= $(INCDIRS:%=-I %)

#Compilation rules
all: nc

bc: byte-code

byte-code: .depend $(OBJINT) $(SRC:.ml=.cmo)
	$(OCAMLC) \
	\
	$(OCAMLFLAGS) $(OPTIONS) $(LIBS:%=%.cma) -o $(EXEC)     \
	$(OBJBYTE)

nc: native-code

native-code: .depend $(OBJINT) $(SRC:.ml=.cmx)
	$(OCAMLOPT) \
	\
	$(OCAMLFLAGS) $(OPTIONS) $(LIBS:%=%.cmxa) -o $(EXEC)    \
	$(OBJNAT)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.PHONY: clean mrproper .depend

.ml.cmo:
	$(OCAMLC) -c $(OCAMLFLAGS) $(OPTIONS) $<

.mli.cmi:
	$(OCAMLOPT) -c $(OCAMLFLAGS) $(OPTIONS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(OCAMLFLAGS) $(OPTIONS) $<

# Clean up
clean:
	rm -f $(OBJBYTE) $(OBJNAT) $(SRC:.ml=.cmi) $(SRC:.ml=.o) *~ \#*\# gui/*~ gui/\#*\#

mrproper: clean
	rm -f $(EXEC) *.tar.bz2 .depend


#Create mli
.ml.mli:
	$(OCAMLC) -i $(OPTIONS) $< > $@

#Packing up
package: mrproper
	tar -cjvC ../ -f Olympe-1.0.tar.bz2 Olympe-1.0/

#Dependencies
.depend:
	ocamldep -I gui/ $(SRC) > .depend

include .depend
