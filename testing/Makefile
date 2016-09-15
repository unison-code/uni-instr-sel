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

PRETTIFY       := ../tools/prettify-json
BUGPOINTSCRIPT := ./compile-from-bc

MINIZINC_GLOBALS_DIR   := chuffed
MINIZINC_BACKEND       := fzn_chuffed
MINIZINC_BACKEND_FLAGS :=

UNI_IS_CMD            := uni-is
CONSTR_CONV_CMD       := ../solvers/presolver/constraints-converter
DOM_MATCHES_CMD       := ../solvers/presolver/find-dominated-matches
ILL_MATCHES_CMD       := ../solvers/presolver/find-illegal-matches
PRUNE_BAD_MATCHES_CMD := ../solvers/presolver/prune-bad-matches
SOLVER_CMD            := ../solvers/minizinc/solver \
                         -g $(MINIZINC_GLOBALS_DIR) \
                         -s "$(MINIZINC_BACKEND) $(MINIZINC_BACKEND_FLAGS)"
SOLVER_TIMELIMIT      := 0 # In seconds; 0 indicates no timelimit

TARGET := mips32

REGRESSION_TESTS_DIR := ./regression



#=============
# BUILD RULES
#=============

.PHONY: build
build:
	cd .. && make

.PHONY: uni-is
uni-is:
	cd .. && make uni-is

.PHONY: hlib
hlib:
	cd .. && make hlib

.PHONY: clean-toolchain
clean-toolchain:
	cd .. && make clean

.PHONY: distclean-toolchain
distclean-toolchain:
	cd .. && make distclean



#=================
# TOOLCHAIN RULES
#=================

.SECONDARY:

include ../toolchain/rules.mk

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

.PHONY: run-regression-tests
run-regression-tests:
	$(MAKE) clean
	for T in $(REGRESSION_TESTS_DIR)/*.ll; do \
		cp $$T . ;\
	    T_NAME=$$(basename $$T) ;\
	    T_ASM=$${T_NAME%.*}.s ;\
		$(MAKE) $$T_ASM ;\
		T_DIFF=`diff -q $$T_ASM $${T%.*}.s` ;\
		if [ -n "$$T_DIFF" ]; then \
			echo "==========================================================" ;\
			echo "=   REGRESSION TEST FAILED: $$T" ;\
			echo "==========================================================" ;\
			exit 1 ;\
		fi ;\
	done
	$(MAKE) clean
	@echo "=========================================================="
	@echo "=   ALL REGRESSION TESTS PASSED"
	@echo "=========================================================="


#=====================
# PLOT AND DUMP RULES
#=====================

%.full-f.dot: %.f.json
	$(UNI_IS_CMD) plot --plot-fun-full-graph -f $< -o $@

%.cfg-f.dot: %.f.json
	$(UNI_IS_CMD) plot --plot-fun-cf-graph -f $< -o $@

%.ssa-f.dot: %.f.json
	$(UNI_IS_CMD) plot --plot-fun-ssa-graph -f $< -o $@

%.cov.dot: %.ce.cc.ae.be.f.json %.ce.cc.ae.be.p.json
	$(UNI_IS_CMD) plot --plot-cover-all-matches --hide-null-instrs \
                  -f $*.ce.cc.ae.be.f.json \
                  -p $*.ce.cc.ae.be.p.json \
                  -o $@

%.pdf: %.dot
	dot -Tpdf $< -o $@

%.all-cover-plots: %.ce.cc.ae.be.f.json %.ce.cc.ae.be.p.json
	$(UNI_IS_CMD) plot --plot-cover-per-match \
                  -f $*.ce.cc.ae.be.f.json \
                  -p $*.ce.cc.ae.be.p.json \
                  -o $*.cov.dot
	for f in $*.cov.*.dot; do \
        $(MAKE) $${f%.*}.pdf; \
        $(RM) $${f%.*}.dot; \
    done

%.model.dump: %.ce.cc.ae.be.f.json \
              %.presolved.aimaps.json \
              %.presolved.ll.model.json
	$(UNI_IS_CMD) make --dump-low-level-model-info \
                  -f $*.ce.cc.ae.be.f.json \
                  -m $*.presolved.ll.model.json \
                  -a $*.presolved.aimaps.json \
                  -o $@

%.sol.dump: %.ce.cc.ae.be.f.json \
            %.presolved.aimaps.json \
            %.presolved.ll.model.json \
            %.presolved.ll.sol.json
	$(UNI_IS_CMD) make --dump-low-level-solution-info \
                  -f $*.ce.cc.ae.be.f.json \
                  -m $*.presolved.ll.model.json \
                  -a $*.presolved.aimaps.json \
                  -s $*.presolved.ll.sol.json \
                  -o $@



#==============
# MISC TARGETS
#==============

%.run-checks: %.ce.cc.ae.be.f.json %.ce.cc.ae.be.p.json
	$(UNI_IS_CMD) check --check-fun-coverage \
                  -f $*.ce.cc.ae.be.f.json \
                  -p $*.ce.cc.ae.be.p.json
	$(UNI_IS_CMD) check --check-fun-location-overlap \
                  -f $*.ce.cc.ae.be.f.json \
                  -p $*.ce.cc.ae.be.p.json

%.run-bugpoint:
	make clean
	bugpoint -compile-custom -compile-command "${BUGPOINTSCRIPT}" \
	         -exec-command "echo OK" -mlimit 0 \
	         $*.ll
	llvm-dis bugpoint-reduced-blocks.bc
	llvm-dis bugpoint-reduced-simplified.bc

%.pretty.json: %.json
	$(PRETTIFY) $< > $@.temp
	mv $@.temp $@
	$(RM) $@.temp

fetch-test-cases:
	cp ../experiments/mediabench/selected/*.ll .



#=================
# AUTO-COMPLETION
#=================

C_TARGETS          := $(wildcard *.c)
LL_TARGETS         := $(wildcard *.ll) \
                      $(addsuffix .ll, $(basename $(C_TARGETS)))
MLL_TARGETS        := $(wildcard *.mll) \
                      $(addsuffix .mll, $(basename $(LL_TARGETS)))
F_JSON_TARGETS     := $(wildcard *.f.json) \
                      $(addsuffix .f.json, $(basename $(MLL_TARGETS)))
FUN_NAMES          := $(basename $(basename $(F_JSON_TARGETS)))

FUN_ASM_TARGETS    := $(addsuffix .s, $(FUN_NAMES))
FUN_PLOTS_TARGETS  := $(addsuffix .cov.pdf, $(FUN_NAMES)) \
                           $(addsuffix .all-cover-plots, $(FUN_NAMES))
FUN_MDUMP_TARGETS  := $(addsuffix .model.dump, $(FUN_NAMES))
FUN_SDUMP_TARGETS := $(addsuffix .sol.dump, $(FUN_NAMES))
FUN_CHECK_TARGETS  := $(addsuffix .run-checks, $(FUN_NAMES))
FUN_BUGPOINT_TARGETS := $(addsuffix .run-bugpoint, $(FUN_NAMES))

$(FUN_ASM_TARGETS):
$(FUN_PLOTS_TARGETS):
$(FUN_MDUMP_TARGETS):
$(FUN_SDUMP_TARGETS):
$(FUN_CHECK_TARGETS):
$(FUN_BUGPOINT_TARGETS):
