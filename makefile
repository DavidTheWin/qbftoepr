.PHONY : clean cleanexe validationtests skolemizationtests removingfunctionstests folconversiontests lexandparsetests dependencyschemetests

NAME = qbftoepr

OBJ_SIZE_DIR = ../../util/objsize-stub/
OBJ_SIZE_CMX = $(OBJ_SIZE_DIR:%=%objsize.cmx)
LIB_OBJ = ../../obj/lib
LIB_CMX = $(LIB_OBJ:%=%.cmx)
LIB_CMI = $(LIB_OBJ:%=%.cmi)
UNIX_CMXA = unix.cmxa

OCAMLOPT=ocamlopt
OCAMLFLAGS=-I obj/ -I ../../obj/ -I $(OBJ_SIZE_DIR)

OBJ_BASE_NAMES_BEFORE_LEXER = \
  qbf \
  fol_qbf
LEXER_BASE_NAME = lexer_qdimacs
OBJ_BASE_NAMES_AFTER_LEXER = \
  parser_qdimacs \
  io_handling \
  $(NAME) 

OBJ_BEFORE_LEXER = $(OBJ_BASE_NAMES_BEFORE_LEXER:%=obj/%.cmx)
OBJ_LEXER = $(LEXER_BASE_NAME:%=obj/%.cmx)
OBJ_AFTER_LEXER = $(OBJ_BASE_NAMES_AFTER_LEXER:%=obj/%.cmx)

OBJ = $(OBJ_BEFORE_LEXER) $(OBJ_LEXER) $(OBJ_AFTER_LEXER)

$(LIB_CMI) : ../lib.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c $<

$(LIB_CMX) : ../lib.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c $<

src/parser_qdimacs.mli src/parser_qdimacs.ml : src/parser_qdimacs.mly
	ocamlyacc src/parser_qdimacs.mly

src/lexer_qdimacs.ml : src/lexer_qdimacs.mll
	ocamllex -q $<

obj/%.cmi : src/%.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c $<

obj/%.cmx : src/%.ml 
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c $< $(UNIX_CMXA) $(OBJ_SIZE_CMX) $(LIB_CMX)

$(NAME): \
  $(LIB_CMI) \
  $(LIB_CMX) \
  src/parser_qdimacs.ml \
  src/lexer_qdimacs.ml \
  $(OBJ_BEFORE_LEXER) \
  obj/parser_qdimacs.cmi \
  $(OBJ_LEXER) \
  $(OBJ_AFTER_LEXER)
	$(OCAMLOPT) $(OCAMLFLAGS) -o $(NAME) $(UNIX_CMXA) $(OBJ_SIZE_CMX) $(LIB_CMX) $(OBJ)

clean:
	rm -f *.cmo *.cmx *.cmi *.o **/*.cmo **/*.cmx **/*.cmi **/*.o \
	  src/parser_*.ml src/parser_*.mli src/lexer_*.ml

cleanexe:
	rm -f $(NAME) validationtests skolemizationtests removingfunctionstests dependencyschemetests folconversiontests lexandparsetests

lexandparsetests: obj/qbf.cmx obj/fol_qbf.cmx src/parser_qdimacs.ml src/lexer_qdimacs.ml obj/parser_qdimacs.cmi obj/lexer_qdimacs.cmx obj/parser_qdimacs.cmx obj/io_handling.cmx
	ocamlfind $(OCAMLOPT) $(OCAMLFLAGS) -o \
	  lexandparsetests \
	  -package oUnit \
	  -linkpkg \
	  -g qbf.cmx fol_qbf.cmx lexer_qdimacs.cmx parser_qdimacs.cmx io_handling.cmx tests/lex_and_parse_tests.ml

validationtests: obj/qbf.cmx
	ocamlfind $(OCAMLOPT) $(OCAMLFLAGS) -o \
	  validationtests \
	  -package oUnit \
	  -linkpkg \
	  -g qbf.cmx tests/qbf_validation_tests.ml

folconversiontests: obj/qbf.cmx obj/fol_qbf.cmx
	ocamlfind $(OCAMLOPT) $(OCAMLFLAGS) -o \
	  folconversiontests \
	  -package oUnit \
	  -linkpkg \
	  -g $(UNIX) $(OBJ_SIZE_CMX) $(LIB_CMX) qbf.cmx fol_qbf.cmx tests/raising_to_fol_tests.ml

dependencyschemetests: obj/qbf.cmx obj/fol_qbf.cmx
	ocamlfind $(OCAMLOPT) $(OCAMLFLAGS) -o \
	  dependencyschemetests\
	  -package oUnit \
	  -linkpkg \
	  -g $(UNIX) $(OBJ_SIZE_CMX) $(LIB_CMX) qbf.cmx fol_qbf.cmx tests/dependency_scheme_tests.ml

skolemizationtests: obj/qbf.cmx obj/fol_qbf.cmx
	ocamlfind $(OCAMLOPT) $(OCAMLFLAGS) -o \
	  skolemizationtests \
	  -package oUnit \
	  -linkpkg \
	  -g $(UNIX) $(OBJ_SIZE_CMX) $(LIB_CMX) qbf.cmx fol_qbf.cmx tests/skolemization_tests.ml

removingfunctionstests: obj/qbf.cmx obj/fol_qbf.cmx
	ocamlfind $(OCAMLOPT) $(OCAMLFLAGS) -o \
	  removingfunctionstests \
	  -package oUnit \
	  -linkpkg \
	  -g $(UNIX) $(OBJ_SIZE_CMX) $(LIB_CMX) qbf.cmx fol_qbf.cmx tests/removing_functions_tests.ml

runtests: 
	mkdir -p test_reports
	./$(testname) > test_reports/$(testname)_report
	less test_reports/$(testname)_report
