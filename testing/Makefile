#==========
# SETTINGS
#==========

HLIBPATH := ../hlib

GHC      := ghc
GHCFLAGS += -Wall -i$(HLIBPATH)

CLANG    := /home/ghb/programs/llvm-3.4-build/Debug+Asserts/bin/clang
LLVMDIR  := /home/ghb/projects/zapbar/llvm-build/Release+Asserts
OPT      := $(LLVMDIR)/bin/opt
AEFMLIB  := $(LLVMDIR)/lib/AttachExecFreqMetadata.so
UNI      := ../uni-is/uni-is
SOLVER   := ../solvers/minizinc/solver
PRETTIFY := ../tools/prettify-json

TARGET := mips32



#=================
# TOOLCHAIN RULES
#=================

.SECONDARY:

.PHONY: build-toolchain
build-toolchain:
	cd .. && make

.PHONY: clean-toolchain
clean-toolchain:
	cd .. && make clean

.PHONY: distclean-toolchain
distclean-toolchain:
	cd .. && make distclean

%.pretty.json: %.json
	$(PRETTIFY) $< > $@

%.ll: %.c
	$(CLANG) -emit-llvm -S $< -o $@

%.mll: %.ll
	$(OPT) -load $(AEFMLIB) -mem2reg -attach-exec-freq-metadata -S $< -o $@

%.f.json: %.mll
	$(UNI) make --construct-fun-from-llvm -f $< -o $@

%.ce.f.json: %.f.json
	$(UNI) transform --copy-extend-fun -f $< -o $@

%.ce.be.f.json: %.ce.f.json
	$(UNI) transform --branch-extend-fun -f $< -o $@

%.p.json: %.ce.be.f.json
	$(UNI) make --compute-pattern-matchset -t $(TARGET) -f $< -o $@

%.hl.model.json: %.ce.be.f.json %.p.json
	$(UNI) make --construct-hl-cp-model -f $*.ce.be.f.json -p $*.p.json -o $@

%.aimaps.json: %.ce.be.f.json %.p.json
	$(UNI) make --compute-array-index-maplists \
      -f $*.ce.be.f.json -p $*.p.json -o $@

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

.PHONY: clean
clean:
	$(RM) *.json
	$(RM) *.s
	$(RM) *.dot
	$(RM) *.pdf



#============
# PLOT RULES
#============

%.full-f.dot: %.f.json
	$(UNI) plot --plot-fun-full-graph -f $< -o $@

%.cfg-f.dot: %.f.json
	$(UNI) plot --plot-fun-cf-graph -f $< -o $@

%.ssa-f.dot: %.f.json
	$(UNI) plot --plot-fun-ssa-graph -f $< -o $@

%.cov.dot: %.ce.be.f.json %.p.json
	$(UNI) plot --plot-cover-all-matches -f $*.ce.be.f.json -p $*.p.json -o $@

%.pdf: %.dot
	dot -Tpdf $< -o $@

%.all-cover-plots: %.f.json %.p.json
	$(UNI) plot --plot-cover-per-match \
      -f $*.ce.be.f.json -p $*.p.json -o $*.cov.dot
	for f in $*.cov.*.dot; do make $${f%.*}.pdf; done
