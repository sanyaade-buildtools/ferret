# canopy makefile
#

# ----------------------------------------------------------------------------
# executable programs

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLMKTOP=ocamlmktop
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

# ----------------------------------------------------------------------------
# compiler and linker options

OPTS=-thread
COPTS=$(OPTS) -c -i
MKOPTS=$(OPTS) -ccopt -O3 -inline 3
LIBS=unix threads str

# ----------------------------------------------------------------------------
# final executable names

OUT=runferret
TOP=ferret

# ----------------------------------------------------------------------------
# input module files

PRIMS=core math series io strings time prims
MODULES=parsec lexer atom word mvar cell reader interp $(PRIMS) term main

# ----------------------------------------------------------------------------
# source files with proper extensions added

ML   := $(addsuffix .ml, $(MODULES))
MLI  := $(addsuffix .mli, $(MODULES))
CMO  := $(addsuffix .cmo, $(MODULES))
CMI  := $(addsuffix .cmi, $(MODULES))
CMX  := $(addsuffix .cmx, $(MODULES))
OBJ  := $(addsuffix .o, $(MODULES))

# ----------------------------------------------------------------------------
# external libraries

CMA  := $(addsuffix .cma, $(LIBS))
CMXA := $(addsuffix .cmxa, $(LIBS))

# ----------------------------------------------------------------------------
# top-level rules

all:		$(OUT)
top:		$(TOP)

# ----------------------------------------------------------------------------
# clean-up rule

clean:
		rm -f $(OUT) $(TOP) $(MLI) $(CMI) $(CMO) $(CMX) $(OBJ) .depend

# ----------------------------------------------------------------------------
# build rules

$(OUT):		.depend $(CMI) $(CMX)
		$(OCAMLOPT) $(CMXA) $(MKOPTS) $(CMX) -o $(OUT)

$(TOP):		.depend $(CMI) $(CMO)
		$(OCAMLMKTOP) $(CMO) -o $(TOP)

%.mli:		%.ml
		$(OCAMLOPT) $(COPTS) $< > $@

%.cmi:		%.mli
		$(OCAMLOPT) $(OPTS) $<

%.cmx:		%.ml
		$(OCAMLOPT) $(MKOPTS) -c $<

%.cmo:		%.ml
		$(OCAMLC) $(MKOPTS) -c $<

.depend:	$(ML)
		$(OCAMLDEP) *.mli *.ml > .depend
