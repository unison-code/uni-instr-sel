#==========
# SETTINGS
#==========

HLIBPATH := ../hlib

GHC      := ghc
GHCFLAGS += -Wall -i$(HLIBPATH)

UNI      := ../uni-is/uni-is
PS       := ../param-splitter/param-splitter
SOLVER   := ../solvers/minizinc/solver
PRETTIFY := ../tools/prettify-json


#===============
# GENERIC RULES
#===============

.SECONDARY:

%.pretty.json: %.json
	$(PRETTIFY) $< > $@

%.f.json: %.ll
	$(UNI) process-llvm-ir -f $< > $@

%.ce.f.json: %.f.json
	$(UNI) transform --copy-extend-fg -f $< > $@

%.be.f.json: %.f.json
	$(UNI) transform --branch-extend-fg -f $< > $@

%.mips32.m.json: %.f.json
	$(UNI) pattern-match -t mips32.ce -f $< > $@

%.params.json: %.f.json %.mips32.m.json
	$(UNI) make-cp-model -f $< --match-file $*.mips32.m.json > $@

%.model.json %.post.json: %.params.json
	$(PS) --mpf=$@ --ppf=$*.post.json $<

%.sol.json: %.model.json
	$(SOLVER) -o $@ $<

%.f.dot: %.f.json
	$(UNI) plot --plot-fg -f $< > $@

%.mips32.cov.dot: %.f.json %.mips32.m.json
	$(UNI) plot --plot-fg-cov -f $< --match-file $*.mips32.m.json > $@

%.mips32.ce.cov.dot: %.ce.f.json %.mips32.ce.m.json
	$(UNI) plot --plot-fg-cov -f $< --match-file $*.mips32.ce.m.json > $@

%.pdf: %.dot
	dot -Tpdf $< -o $@

.PHONY: clean
clean:
	find . -name "*.hi" -type f -exec rm {} \;
	find . -name "*.o" -type f -exec rm {} \;

.PHONY: veryclean
veryclean: clean
	-rm parser



#=================
# TEMPORARY RULES
#=================

.PHONY: param-test
param-test:
	$(GHC) $(GHCFLAGS) param-test.hs

.PHONY: run-param-test
run-param-test: param-test
	./param-test

.PHONY: match-test
match-test:
	$(GHC) $(GHCFLAGS) match-test.hs

.PHONY: run-match-test
run-match-test: match-test
	./match-test

.PHONY: test-frontend
test-frontend:
	$(GHC) $(GHCFLAGS) test-llvm.hs
	./test-llvm test.ll
	dot -Tpdf test.dot -o test.pdf

parser:
	$(GHC) $(GHCFLAGS) parser.hs
