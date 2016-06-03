#==========
# SETTINGS
#==========

HLIBPATH := ../hlib

GHC      := ghc
GHCFLAGS += -Wall -i$(HLIBPATH)

CLANG    := /home/ghb/programs/llvm-3.4-build/Release+Asserts/bin/clang
LLVMDIR  := /home/ghb/projects/zapbar/llvm-build/Release+Asserts
OPT      := $(LLVMDIR)/bin/opt
AEFMLIB  := $(LLVMDIR)/lib/AttachExecFreqMetadata.so

export MINIZINC_BACKEND := mzn-cpx

UNI              := uni-is
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

.PHONY: uni-is
uni-is:
	cd .. && make uni-is

.PHONY: hlib
hlib:
	cd .. && make hlib

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

%.ce.cc.ae.f.json: %.ce.cc.f.json
	$(UNI) transform --alternative-extend-fun -f $< -o $@

%.ce.cc.ae.be.f.json: %.ce.cc.ae.f.json
	$(UNI) transform --branch-extend-fun -f $< -o $@

%.p.json: %.f.json
	$(UNI) make --compute-pattern-matchset -t $(TARGET) -f $< -o $@

%.hl.model-no-op.json: %.ce.cc.ae.be.f.json %.ce.cc.ae.be.p.json
	$(UNI) make --construct-hl-cp-model-no-op \
      -f $*.ce.cc.ae.be.f.json -p $*.ce.cc.ae.be.p.json -o $@

%.hl.model-w-op.json: %.hl.model-no-op.json %.ce.cc.ae.be.p.json
	$(UNI) make --construct-hl-cp-model-w-op \
      -m $*.hl.model-no-op.json -p $*.ce.cc.ae.be.p.json -o $@

%.aimaps.json: %.ce.cc.ae.be.f.json %.hl.model-w-op.json
	$(UNI) make --compute-array-index-maplists \
      -f $*.ce.cc.ae.be.f.json -m $*.hl.model-w-op.json -o $@

%.ll.model.json: %.hl.model-w-op.json %.aimaps.json
	$(UNI) transform --lower-hl-cp-model \
      -m $*.hl.model-w-op.json -a $*.aimaps.json -o $@

%.ll.sol.json: %.ll.model.json
	$(SOLVER_CMD) -t $(SOLVER_TIMELIMIT) -o $@ $<

%.hl.sol.json: %.hl.model-w-op.json %.ll.sol.json %.aimaps.json
	$(UNI) transform --raise-ll-cp-solution \
      -m $*.hl.model-w-op.json -s $*.ll.sol.json -a $*.aimaps.json -o $@

%.s: %.hl.model-w-op.json %.hl.sol.json
	$(UNI) make --generate-asm -m $*.hl.model-w-op.json -s $*.hl.sol.json -o $@

.PHONY: clean
clean:
	$(RM) *.json
	$(RM) *.mzn
	$(RM) *.s
	$(RM) *.stats
	$(RM) *.dump
	$(RM) *.dot
	$(RM) *.pdf

.PHONY: clean-toolchain
clean-toolchain:
	cd .. && make clean

.PHONY: distclean-toolchain
distclean-toolchain:
	cd .. && make distclean



#=====================
# PLOT AND DUMP RULES
#=====================

%.full-f.dot: %.f.json
	$(UNI) plot --plot-fun-full-graph -f $< -o $@

%.cfg-f.dot: %.f.json
	$(UNI) plot --plot-fun-cf-graph -f $< -o $@

%.ssa-f.dot: %.f.json
	$(UNI) plot --plot-fun-ssa-graph -f $< -o $@

%.cov.dot: %.ce.cc.ae.be.f.json %.ce.cc.ae.be.p.json
	$(UNI) plot --plot-cover-all-matches \
      -f $*.ce.cc.ae.be.f.json -p $*.ce.cc.ae.be.p.json -o $@

%.pdf: %.dot
	dot -Tpdf $< -o $@

%.all-cover-plots: %.ce.cc.ae.be.f.json %.ce.cc.ae.be.p.json
	$(UNI) plot --plot-cover-per-match \
      -f $*.ce.cc.ae.be.f.json -p $*.ce.cc.ae.be.p.json -o $*.cov.dot
	for f in $*.cov.*.dot; do \
	  make $${f%.*}.pdf;      \
	  $(RM) $${f%.*}.dot;     \
    done

%.dump: %.ce.cc.ae.be.f.json %.ce.cc.ae.be.p.json %.aimaps.json
	$(UNI) make --dump-array-index-maplists-info \
      -f $*.ce.cc.ae.be.f.json -p $*.ce.cc.ae.be.p.json -a $*.aimaps.json -o $@



#==============
# MISC TARGETS
#==============

%.pretty.json: %.json
	$(PRETTIFY) $< > $@

fetch-test-cases:
	cp ../experiments/mediabench/selected/*.ll .
	rename 's/\.ll$$/.mll/' *.ll



#=================
# AUTO-COMPLETION
#=================

C_TARGETS         := $(wildcard *.c)
LL_TARGETS        := $(wildcard *.ll) \
                     $(addsuffix .ll, $(basename $(C_TARGETS)))
MLL_TARGETS       := $(wildcard *.mll) \
                     $(addsuffix .mll, $(basename $(LL_TARGETS)))
F_JSON_TARGETS    := $(wildcard *.f.json) \
                     $(addsuffix .f.json, $(basename $(MLL_TARGETS)))
FUN_NAMES         := $(basename $(basename $(F_JSON_TARGETS)))

FUN_ASM_TARGETS   := $(addsuffix .s, $(FUN_NAMES))
FUN_PLOTS_TARGETS := $(addsuffix .cov.pdf, $(FUN_NAMES)) \
                          $(addsuffix .all-cover-plots, $(FUN_NAMES))
FUN_MDUMP_TARGETS := $(addsuffix .dump, $(FUN_NAMES))

$(FUN_ASM_TARGETS):
$(FUN_PLOTS_TARGETS):
$(FUN_MDUMP_TARGETS):
