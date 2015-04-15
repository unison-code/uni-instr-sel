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
MINIZINC_BACKEND := minizinc
SOLVER_CMD       := ../solvers/minizinc/solver -s $(MINIZINC_BACKEND)
SOLVER_TIMELIMIT := 0 # In milliseconds; 0 indicates no timelimit
PRETTIFY := ../tools/prettify-json

TARGET := mips32



#=================
# TOOLCHAIN RULES
#=================

.SECONDARY:

.PHONY: build
build:
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

%.ce.cc.f.json: %.ce.f.json
	$(UNI) transform --combine-consts-in-fun -f $< -o $@

%.ce.cc.be.f.json: %.ce.cc.f.json
	$(UNI) transform --branch-extend-fun -f $< -o $@

%.p.json: %.f.json
	$(UNI) make --compute-pattern-matchset -t $(TARGET) -f $< -o $@

%.hl.model.json: %.ce.cc.be.f.json %.ce.cc.be.p.json
	$(UNI) make --construct-hl-cp-model \
      -f $*.ce.cc.be.f.json -p $*.ce.cc.be.p.json -o $@

%.aimaps.json: %.ce.cc.be.f.json %.ce.cc.be.p.json
	$(UNI) make --compute-array-index-maplists \
      -f $*.ce.cc.be.f.json -p $*.ce.cc.be.p.json -o $@

%.ll.model.json: %.hl.model.json %.aimaps.json
	$(UNI) transform --lower-hl-cp-model \
      -m $*.hl.model.json -a $*.aimaps.json -o $@

%.ll.sol.json: %.ll.model.json
	$(SOLVER_CMD) -t $(SOLVER_TIMELIMIT) -o $@ $<

%.hl.sol.json: %.ll.sol.json %.aimaps.json
	$(UNI) transform --raise-ll-cp-solution \
      -s $*.ll.sol.json -a $*.aimaps.json -o $@

%.s: %.hl.model.json %.hl.sol.json
	$(UNI) make --generate-asm -m $*.hl.model.json -s $*.hl.sol.json -o $@

.PHONY: clean
clean:
	$(RM) *.mll
	$(RM) *.json
	$(RM) *.s
	$(RM) *.stats
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

%.cov.dot: %.f.json %.p.json
	$(UNI) plot --plot-cover-all-matches -f $*.f.json -p $*.p.json -o $@

%.pdf: %.dot
	dot -Tpdf $< -o $@

%.all-cover-plots: %.f.json %.p.json
	$(UNI) plot --plot-cover-per-match \
      -f $*.f.json -p $*.p.json -o $*.cov.dot
	for f in $*.cov.*.dot; do make $${f%.*}.pdf; done
