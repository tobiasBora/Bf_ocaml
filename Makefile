# Compiler :
CC=ocamlopt
LDFLAGS=
CFLAGS=

MAIN=bf_ocaml.ml
SOURCES=
PACKAGES=unix

OBJECTS=$(SOURCES)
OUTPUT=$(MAIN:.ml=.bin)

# The -p option is for debug with gprof
all: $(OBJECTS)
	ocamlfind $(CC) $(CFLAGS) -package `echo "$(PACKAGES)" | sed 's/ / -package /g'` -linkpkg $(SOURCES) $(MAIN) -o $(OUTPUT)

clean:
	rm *.cmi *.cmx *.o $(OUTPUT)
