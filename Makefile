# canopy makefile
#

# ----------------------------------------------------------------------------
# executable programs

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLMKTOP=ocamlmktop
OCAMLDEP=ocamldep
OCAMLFIND=ocamlfind
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

# ----------------------------------------------------------------------------
# final executable names

OUT=runferret
TOP=ferret

# ----------------------------------------------------------------------------
# third-party, site-lib modules

OCAMLNET=equeue netstring netsys netclient smtp

# ----------------------------------------------------------------------------
# compiler and linker options

PKGS=-package "$(OCAMLNET)"
LINK=-linkpkg -inline 3 -ccopt -O2
OPTS=-pp camlp4o -thread

# ----------------------------------------------------------------------------
# input module files

SOURCES= \
  parsec \
  lexer \
  atom \
  mvar \
  cell \
  compiler \
  interp \
  core \
  prims \
  main
#  core \
#  process \
#  math \
#  series \
#  io \
#  strings \
#  time \
#  prims \

# ----------------------------------------------------------------------------
# source files with proper extensions added

ML   := $(addsuffix .ml, $(SOURCES))
MLI  := $(addsuffix .mli, $(SOURCES))
CMO  := $(addsuffix .cmo, $(SOURCES))
CMI  := $(addsuffix .cmi, $(SOURCES))
CMX  := $(addsuffix .cmx, $(SOURCES))
OBJ  := $(addsuffix .o, $(SOURCES))

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

$(OUT):		$(ML)
		$(OCAMLFIND) $(OCAMLOPT) $(OPTS) $(PKGS) $(LINK) $(ML) -o $(OUT)

$(TOP):		$(ML)
		$(OCAMLFIND) $(OCAMLMKTOP) $(OPTS) $(PKGS) $(ML) -o $(TOP)

%.mli:		%.ml
		$(OCAMLFIND) $(OCAMLOPT) $(OPTS) $(PKGS) -c -i $< > $@

%.cmi:		%.mli
		$(OCAMLFIND) $(OCAMLOPT) $(OPTS) $(PKGS) $<

%.cmx:		%.ml
		$(OCAMLFIND) $(OCAMLOPT) $(OPTS) $(PKGS) -c $<

%.cmo:		%.ml
		$(OCAMLFIND) $(OCAMLC) $(OPTS) $(PKGS) -c $<

.depend:	$(ML)
		$(OCAMLDEP) *.mli *.ml > .depend
