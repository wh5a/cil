# OCaml-batteries
#OCAMLC     = ocamlfind batteries/ocamlc
#OCAMLFLAGS = -g -package cil

# Faster without batteries
OCAMLC     = ocamlfind ocamlc
# Preprocess with "the whitespace thing".
OCAMLFLAGS = -pp ocaml+twt -g -package cil -package str -linkpkg -w Ame -annot
OCAMLCI = ocamlfind ocamlc -package cil -package str -linkpkg -w Ame -annot
ifdef V		# usage: make V=1
OCAMLFLAGS += -verbose
endif

SRC = analyze.ml helper.ml ext.ml
MLI = analyze.mli helper.mli

ext : $(SRC) $(MLI)
	$(OCAMLCI) -c $(MLI)
	$(OCAMLC) $(OCAMLFLAGS) $(SRC) -o ext

clean : 
	$(RM) -f *.cmi *.cmx *.o *.cmo ext *~ a.out *.annot
