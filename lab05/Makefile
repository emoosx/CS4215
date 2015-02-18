INPUT = $(shell find . -name *.epl)
FILENAMES = $(notdir $(INPUT))
OUTPUT = $(INPUT:.epl=.out)

all: clean compile $(OUTPUT)
	@echo "done"

clean:
	rm *.out

compile: ePL.ml ePL_compile.ml eVM_exec.ml
	./bincomp5.sh

%.out: %.epl ePL_compile.ml eVM_exec.ml
	./eplc $<
	./evm $(basename $<) > $@

check: compile $(OUTPUT)
	@echo " #### Testing for equality ####"
	for i in $(basename $(FILENAMES)); do \
	 	echo "$$i diff result--";\
	 	diff test/$$i.out test/out.$$i | colordiff;\
	done
	@echo "### Done Testing ###"
