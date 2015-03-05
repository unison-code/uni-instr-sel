#==========
# SETTINGS
#==========

HLIBPATH := ../hlib

GHC      := ghc
GHCFLAGS += -Wall -i$(HLIBPATH)

UNI      := ../uni-is/uni-is
SOLVER   := ../solvers/minizinc/solver
PRETTIFY := ../tools/prettify-json


#===============
# GENERIC RULES
#===============

.SECONDARY:

.PHONY: build-toolchain
build-toolchain:
	cd ../uni-is && make
	cd ../solvers && make

%.pretty.json: %.json
	$(PRETTIFY) $< > $@

%.f.json: %.ll
	$(UNI) make --construct-fun-from-llvm -f $< -o $@

%.ce.f.json: %.f.json
	$(UNI) transform --copy-extend-fun -f $< -o $@

%.be.f.json: %.f.json
	$(UNI) transform --branch-extend-fun -f $< -o $@

%.mips32.p.json: %.f.json
	$(UNI) make --compute-pattern-matchset -t mips32.ce -f $< -o $@

%.hl.model.json: %.f.json %.mips32.p.json
	$(UNI) make --construct-hl-cp-model -f $*.f.json -p $*.mips32.p.json -o $@

%.aimaps.json: %.f.json %.mips32.p.json
	$(UNI) make --compute-array-index-maplists \
      -f $*.f.json -p $*.mips32.p.json -o $@

%.ll.model.json: %.hl.model.json %.aimaps.json
	$(UNI) transform --lower-hl-cp-model \
      -m $*.hl.model.json -a $*.aimaps.json -o $@

%.ll.sol.json: %.ll.model.json
	$(SOLVER) -o $@ $<

%.hl.sol.json: %.ll.sol.json %.aimaps.json
	$(UNI) transform --raise-ll-cp-solution \
      -s $*.ll.sol.json -a $*.aimaps.json -o $@

%.s: %.hl.model.json %.hl.sol.json
	$(UNI) make --generate-asm -m $*.hl.model.json -s $*.hl.sol.json -o $@

%.full-f.dot: %.f.json
	$(UNI) plot --plot-fun-full-graph -f $< -o $@

%.cfg-f.dot: %.f.json
	$(UNI) plot --plot-fun-cf-graph -f $< -o $@

%.ssa-f.dot: %.f.json
	$(UNI) plot --plot-fun-ssa-graph -f $< -o $@

%.mips32.cov.dot: %.f.json %.mips32.p.json
	$(UNI) plot --plot-cover-all-matches -f $< -p $*.mips32.p.json -o $@

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
