MODULES=main connectx authors connectai

OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play: build
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip connect4.zip *.ml* *.json *.txt _tags Makefile