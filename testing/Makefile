#
#  Main authors:
#    Gabriel Hjort Blindell <ghb@kth.se>
#
#  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are met:
#  1. Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#  3. Neither the name of the copyright holder nor the names of its contributors
#     may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
#  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
#  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
#  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
#  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
#  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

HLIBPATH := ../hlib

GHC      := ghc
GHCFLAGS += -Wall -i$(HLIBPATH)

LLVMDIR  := /home/ghb/projects/zapbar/llvm-int-build
CLANG    := $(LLVMDIR)/bin/clang
OPT      := $(LLVMDIR)/bin/opt
LSLIB    := $(LLVMDIR)/lib/LibLowerSelect.so
AEFMLIB  := $(LLVMDIR)/lib/LibAttachExecFreqMetadata.so

MINIZINC_BACKEND := fzn_chuffed
MINIZINC_GLOBALS_DIR := chuffed

UNI              := uni-is
CONSTR_CONV_CMD  := ../solvers/presolver/constraints-converter
DOM_MATCHES_CMD  := ../solvers/presolver/find-dominated-matches
ILL_MATCHES_CMD  := ../solvers/presolver/find-illegal-matches
PRUNE_BAD_MATCHES_CMD := ../solvers/presolver/prune-bad-matches
SOLVER_CMD       := ../solvers/minizinc/solver -s $(MINIZINC_BACKEND) \
	                -g $(MINIZINC_GLOBALS_DIR)
SOLVER_TIMELIMIT := 0 # In seconds; 0 indicates no timelimit
PRETTIFY         := ../tools/prettify-json
BUGPOINTSCRIPT   := ./compile-from-bc

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

%.reg.ll: %.ll
	$(OPT) -mem2reg -S $< -o $@

%.reg.low.ll: %.reg.ll
	$(OPT) -load $(LSLIB) -lowerselect -lowerswitch -S $< -o $@

%.reg.low.freq.ll: %.reg.low.ll
	$(OPT) -load $(AEFMLIB) -attach-exec-freq-metadata -S $< -o $@

%.f.json: %.reg.low.freq.ll
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

%.ce.cc.ae.be.dom.json: %.ll.model.json
	$(eval TEMPFILE := $(shell mktemp))
	$(CONSTR_CONV_CMD) $< > $(TEMPFILE)
	$(DOM_MATCHES_CMD) $(TEMPFILE) > $@
	$(RM) $(TEMPFILE)

%.ce.cc.ae.be.ill.json: %.ll.model.json
	$(ILL_MATCHES_CMD) $< > $@

%.ce.cc.ae.be.presolved.p.json: %.ce.cc.ae.be.p.json \
                                %.ce.cc.ae.be.dom.json \
                                %.ce.cc.ae.be.ill.json \
                                %.aimaps.json
	$(PRUNE_BAD_MATCHES_CMD) -d $*.ce.cc.ae.be.dom.json \
                             -i $*.ce.cc.ae.be.ill.json \
                             -p $*.ce.cc.ae.be.p.json \
                             -a $*.aimaps.json \
                             > $@

%.presolved.hl.model-no-op.json: %.ce.cc.ae.be.f.json \
                                 %.ce.cc.ae.be.presolved.p.json
	$(UNI) make --construct-hl-cp-model-no-op \
           -f $*.ce.cc.ae.be.f.json \
           -p $*.ce.cc.ae.be.presolved.p.json \
           -o $@

%.hl.model-no-op.json: %.ce.cc.ae.be.f.json %.ce.cc.ae.be.p.json
	$(UNI) make --construct-hl-cp-model-no-op \
           -f $*.ce.cc.ae.be.f.json \
           -p $*.ce.cc.ae.be.p.json \
           -o $@

%.presolved.hl.model-w-op.json: %.presolved.hl.model-no-op.json \
                                %.ce.cc.ae.be.presolved.p.json
	$(UNI) make --construct-hl-cp-model-w-op \
           -m $*.presolved.hl.model-no-op.json \
           -p $*.ce.cc.ae.be.presolved.p.json \
           -o $@

%.hl.model-w-op.json: %.hl.model-no-op.json %.ce.cc.ae.be.p.json
	$(UNI) make --construct-hl-cp-model-w-op \
           -m $*.hl.model-no-op.json \
           -p $*.ce.cc.ae.be.p.json \
           -o $@

%.presolved.aimaps.json: %.ce.cc.ae.be.f.json %.presolved.hl.model-w-op.json
	$(UNI) make --compute-array-index-maplists \
           -f $*.ce.cc.ae.be.f.json \
           -m $*.presolved.hl.model-w-op.json \
           -o $@

%.aimaps.json: %.ce.cc.ae.be.f.json %.hl.model-w-op.json
	$(UNI) make --compute-array-index-maplists \
           -f $*.ce.cc.ae.be.f.json \
           -m $*.hl.model-w-op.json \
           -o $@

%.ll.model.json: %.hl.model-w-op.json %.aimaps.json
	$(UNI) transform --lower-hl-cp-model \
           -m $*.hl.model-w-op.json \
           -a $*.aimaps.json \
           -o $@

%.ll.sol.json: %.ll.model.json
	$(SOLVER_CMD) -t $(SOLVER_TIMELIMIT) -o $@ $<

%.hl.sol.json: %.hl.model-w-op.json %.ll.sol.json %.aimaps.json
	$(UNI) transform --raise-ll-cp-solution \
           -m $*.hl.model-w-op.json \
           -s $*.ll.sol.json \
           -a $*.aimaps.json \
           -o $@

%.s: %.presolved.hl.model-w-op.json %.presolved.hl.sol.json
	$(UNI) make --generate-asm \
           -m $*.presolved.hl.model-w-op.json \
           -s $*.presolved.hl.sol.json \
           -o $@

.PHONY: clean
clean:
	$(RM) *.reg.ll
	$(RM) *.reg.low.ll
	$(RM) *.reg.low.freq.ll
	$(RM) *.json
	$(RM) *.mzn
	$(RM) *.s
	$(RM) *.stats
	$(RM) *.dump
	$(RM) *.dot
	$(RM) *.pdf
	$(RM) *.bc

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
	$(UNI) plot --plot-cover-all-matches --hide-null-instrs \
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

%.dump: %.ce.cc.ae.be.f.json %.ll.model.json %.aimaps.json
	$(UNI) make --dump-low-level-model-info \
      -f $*.ce.cc.ae.be.f.json -m $*.ll.model.json -a $*.aimaps.json -o $@



#==============
# MISC TARGETS
#==============

%.run-checks: %.ce.cc.ae.be.f.json %.ce.cc.ae.be.p.json
	$(UNI) check --check-fun-coverage \
      -f $*.ce.cc.ae.be.f.json -p $*.ce.cc.ae.be.p.json
	$(UNI) check --check-fun-location-overlap \
      -f $*.ce.cc.ae.be.f.json -p $*.ce.cc.ae.be.p.json

%.run-bugpoint:
	make clean
	bugpoint -compile-custom -compile-command "${BUGPOINTSCRIPT}" \
	         -exec-command "echo OK" -mlimit 0 \
	         $*.ll
	llvm-dis bugpoint-reduced-blocks.bc
	llvm-dis bugpoint-reduced-simplified.bc

%.pretty.json: %.json
	$(eval TEMPFILE := $(shell mktemp))
	$(PRETTIFY) $< > $(TEMPFILE)
	mv $(TEMPFILE) $@
	$(RM) $(TEMPFILE)

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
FUN_CHECK_TARGETS := $(addsuffix .run-checks, $(FUN_NAMES))
FUN_BUGPOINT_TARGETS := $(addsuffix .run-bugpoint, $(FUN_NAMES))

$(FUN_ASM_TARGETS):
$(FUN_PLOTS_TARGETS):
$(FUN_MDUMP_TARGETS):
$(FUN_CHECK_TARGETS):
$(FUN_BUGPOINT_TARGETS):
