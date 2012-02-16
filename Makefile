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

OPTS=-thread -package "$(OCAMLNET)" -linkpkg -ccopt -O2 -inline 3

# ----------------------------------------------------------------------------
# input module files

SOURCES= \
  parsec \
  lexer \
  atom \
  word \
  mvar \
  cell \
  reader \
  interp \
  core \
  process \
  math \
  series \
  io \
  strings \
  time \
  prims \
  term \
  main \

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

$(OUT):		.depend $(ML)
		$(OCAMLFIND) $(OCAMLOPT) $(OPTS) $(ML) -o $(OUT)

$(TOP):		.depend $(ML)
		$(OCAMLFIND) $(OCAMLMKTOP) $(OPTS) $(ML) -o $(TOP)

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
