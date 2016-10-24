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
BUGPOINTSCRIPT := ./bugpoint/compile-from-bc

MINIZINC_GLOBALS_DIR   := chuffed
MINIZINC_BACKEND       := fzn_chuffed
MINIZINC_BACKEND_FLAGS :=

CLANG                 := $(UNI_IS_LLVM_BUILD_DIR)/bin/clang
UNI_IS_CMD            := uni-is
UNI_TARGEN_CMD        := uni-targen
CONSTR_CONV_CMD       := ../solvers/presolver/constraints-converter
DOM_MATCHES_CMD       := ../solvers/presolver/find-dominated-matches
ILL_MATCHES_CMD       := ../solvers/presolver/find-illegal-matches
PRUNE_BAD_MATCHES_CMD := ../solvers/presolver/prune-bad-matches
SOLVER_CMD            := ../solvers/minizinc/solver \
                         -g $(MINIZINC_GLOBALS_DIR) \
                         -s "$(MINIZINC_BACKEND) $(MINIZINC_BACKEND_FLAGS)"
ALT_INSERT_LIMIT      := 1 # 0 indicates no timelimit
SOLVER_TIME_LIMIT     := 0 # In seconds; 0 indicates no timelimit

TARGET := mips32

REGRESSION_TESTS_DIR := ./regression

RED=\033[1;31m
GREEN=\033[1;32m
YELLOW=\033[1;33m
BLUE=\033[1;34m
NC=\033[0m# No color




#=============
# BUILD RULES
#=============

.PHONY: build
build:
	cd .. && make

.PHONY: uni-is
uni-is:
	cd .. && make uni-is

.PHONY: uni-targen
uni-targen:
	cd .. && make uni-targen

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

%.ll: %.c
	$(CLANG) -O1 -emit-llvm -S $< -o $@

include ../toolchain/rules.mk

.PHONY: clean
clean:
	$(RM) *.low.ll
	$(RM) *.low.freq.ll
	$(RM) *.json
	$(RM) *.mzn
	$(RM) *.s
	$(RM) *.stats
	$(RM) *.dump
	$(RM) *.dot
	$(RM) *.pdf
	$(RM) *.bc
	$(RM) tmp.*

.PHONY: run-regression-tests
run-regression-tests:
	@$(MAKE) TARGET=Mips32   run-regression-tests-for-target
	@$(MAKE) TARGET=Hexagon5 run-regression-tests-for-target

.PHONY: run-regression-tests-for-target
run-regression-tests-for-target:
	@echo
	@echo -n "$(BLUE)"
	@echo "=========================================================="
	@echo "=   REGRESSION TESTS FOR TARGET: $(NC)$(TARGET)$(BLUE)"
	@echo "=========================================================="
	@echo -n "$(NC)"
	@echo
	@echo "$(BLUE)NOTE: Solving time is given in brackets$(NC)"
	@echo
	@echo "$(BLUE)* EXECUTING REGRESSION TESTS$:$(NC)"
	@\
	T_TOTAL=0 ;\
	T_PASSED=0 ;\
	T_IGNORED=0 ;\
	T_FAILED=0 ;\
	T_ALL=`cat $(REGRESSION_TESTS_DIR)/test-cases.db` ;\
	T_MAX_LENGTH=0 ;\
	T_TIME_START=`date +%s` ;\
	T_PREP_TIME_TOTAL=0 ;\
	T_SOLV_TIME_TOTAL=0 ;\
	for T in $$T_ALL; do \
	    T_NAME=`basename $$T` ;\
		if [ "$${#T_NAME}" -gt "$$T_MAX_LENGTH" ]; then \
			T_MAX_LENGTH=$${#T_NAME} ;\
	    fi \
    done ;\
	for T in $$T_ALL; do \
	    T_TOTAL=`expr $$T_TOTAL + 1` ;\
	    T_NAME=`basename $$T` ;\
		echo -n "    $$T_NAME" ;\
		for i in `seq $${#T_NAME} $$T_MAX_LENGTH`; do \
			echo -n " " ;\
		done ;\
		TARGET_LC=`echo $(TARGET) | tr A-Z a-z` ;\
		RT_TARGET_DIR=$(REGRESSION_TESTS_DIR)/$$TARGET_LC ;\
		T_ORACLES=`ls $$RT_TARGET_DIR/$${T_NAME%.*}.s.* 2> /dev/null` ;\
		if [ -n "$$T_ORACLES" ]; then \
			TEMPFILE=$$(basename `mktemp`) ;\
			T_LL=$${TEMPFILE}.ll ;\
		    T_ASM=$${TEMPFILE}.s ;\
			cp $$T $$T_LL ;\
			$(MAKE) $$T_ASM > /dev/null 2>&1 ;\
		    T_ASM_TXT=`cat $$T_ASM 2> /dev/null` ;\
			sol_file="$${T_LL%.*}.presolved.hl.sol.json" ;\
			if [ -f $$sol_file ]; then \
				solv_time=`$(REGRESSION_TESTS_DIR)/get-solving-time $$sol_file \
						   2> /dev/null` ;\
				T_SOLV_TIME_TOTAL=`echo "scale=2; \
							 		 t = $$T_SOLV_TIME_TOTAL + $$solv_time; \
									 if (t < 1) print 0; t" \
								   | bc` ;\
				prep_time=`$(REGRESSION_TESTS_DIR)/get-prep-time $$sol_file \
						   2> /dev/null` ;\
				T_PREP_TIME_TOTAL=`echo "scale=2; \
							 		 t = $$T_PREP_TIME_TOTAL + $$prep_time; \
									 if (t < 1) print 0; t" \
								   | bc` ;\
			else \
				solv_time="?" ;\
			fi ;\
			if [ -f "$$T_ASM" ] && [ -n "$$T_ASM_TXT" ]; then \
				T_IS_OK=0 ;\
				for O in $$T_ORACLES; do \
					if [ "$$T_IS_OK" -eq "0" ]; then \
						T_DIFF=`diff $$T_ASM $$O` ;\
						if [ -z "$$T_DIFF" ]; then \
							T_IS_OK=1 ;\
						fi \
					fi \
				done ;\
				if [ "$$T_IS_OK" -ne "0" ]; then \
				    T_PASSED=`expr $$T_PASSED + 1` ;\
				    echo "[$(GREEN)OK$(NC)] [$${solv_time}s]" ;\
				else \
				    T_FAILED=`expr $$T_FAILED + 1` ;\
				    echo "[$(RED)FAILED$(NC)] [$${solv_time}s] (diff)" ;\
					echo "$$T_DIFF" ;\
				fi \
			else \
			    T_FAILED=`expr $$T_FAILED + 1` ;\
			    echo "[$(RED)FAILED$(NC)] [$${solv_time}s] (no output)" ;\
			fi ;\
			rm -f $${TEMPFILE}.* ;\
		else \
		    T_IGNORED=`expr $$T_IGNORED + 1` ;\
			echo "[$(YELLOW)IGNORED$(NC)] (has no oracles)" ;\
		fi ;\
	done ;\
	T_TIME_END=`date +%s` ;\
	T_TIME_TOTAL=`expr $$T_TIME_END - $$T_TIME_START` ;\
	T_TIME_TOTAL_PRETTY=`date -u -d @"$$T_TIME_TOTAL" +'%-Mm %-Ss'` ;\
	T_PREP_TIME_TOTAL=`LC_ALL=en_US.UTF-8 printf '%.*f' 0 \
					   $$T_PREP_TIME_TOTAL` ;\
	T_PREP_TIME_TOTAL_PRETTY=`date -u -d @"$$T_PREP_TIME_TOTAL" +'%-Mm %-Ss'` ;\
	T_SOLV_TIME_TOTAL=`LC_ALL=en_US.UTF-8 printf '%.*f' 0 \
					   $$T_SOLV_TIME_TOTAL` ;\
	T_SOLV_TIME_TOTAL_PRETTY=`date -u -d @"$$T_SOLV_TIME_TOTAL" +'%-Mm %-Ss'` ;\
	T_MISC_TIME_TOTAL=`expr $$T_TIME_TOTAL - \
					   $$T_SOLV_TIME_TOTAL - \
					   $$T_PREP_TIME_TOTAL` ;\
	T_MISC_TIME_TOTAL_PRETTY=`date -u -d @"$$T_MISC_TIME_TOTAL" +'%-Mm %-Ss'` ;\
	echo ;\
	echo -n "$(BLUE)" ;\
	echo "==========================================================" ;\
	echo -n "=   PASSED $(GREEN)$$T_PASSED$(BLUE)" ;\
	if [ "$$T_IGNORED" -ne "0" ]; then \
		echo -n ", IGNORED $(YELLOW)$$T_IGNORED$(BLUE)" ;\
	fi ;\
	if [ "$$T_FAILED" -ne "0" ]; then \
		echo -n ", FAILED $(RED)$$T_FAILED$(BLUE)" ;\
	fi ;\
	echo " of $(NC)$$T_TOTAL$(BLUE) TESTS" ;\
	echo "=" ;\
	echo "=   TOOK: $(NC)$${T_TIME_TOTAL_PRETTY}$(BLUE)" ;\
	echo "=   SOLVING: $(NC)$${T_SOLV_TIME_TOTAL_PRETTY}$(BLUE)" ;\
	echo -n "=   MODEL PREP (FLATTENING): " ;\
	echo "$(NC)$${T_PREP_TIME_TOTAL_PRETTY}$(BLUE)" ;\
	echo "=   OTHER TASKS: $(NC)$${T_MISC_TIME_TOTAL_PRETTY}$(BLUE)" ;\
	echo "==========================================================" ;\
	echo -n "$(NC)" ;\
	echo


#=====================
# PLOT AND DUMP RULES
#=====================

%.full-f.dot: %.ph.lp.ce.cc.be.f.json
	$(UNI_IS_CMD) plot --plot-fun-full-graph -f $< -o $@

%.cfg-f.dot: %.f.ph.lp.ce.cc.be.json
	$(UNI_IS_CMD) plot --plot-fun-cf-graph -f $< -o $@

%.ssa-f.dot: %.f.ph.lp.ce.cc.be.json
	$(UNI_IS_CMD) plot --plot-fun-ssa-graph -f $< -o $@

%.cov.dot: %.ph.lp.ce.cc.be.f.json %.ph.lp.ce.cc.be.ae.p.json
	$(UNI_IS_CMD) plot --plot-cover-all-matches --hide-null-instrs \
                  -f $*.ph.lp.ce.cc.be.f.json \
                  -p $*.ph.lp.ce.cc.be.ae.p.json \
                  -o $@

.PHONY: pattern.dot
pattern.dot:
	$(UNI_IS_CMD) plot --plot-pat-full-graph \
                  -t $(TARGET) \
				  --instruction $(INSTR) \
				  --pattern $(PAT) \
                  -o $@

%.pdf: %.dot
	dot -Tpdf $< -o $@

%.all-cover-plots: %.ph.lp.ce.cc.be.f.json %.ph.lp.ce.cc.be.ae.p.json
	$(UNI_IS_CMD) plot --plot-cover-per-match \
                  -f $*.ph.lp.ce.cc.be.f.json \
                  -p $*.ph.lp.ce.cc.be.ae.p.json \
                  -o $*.cov.dot
	for f in $*.cov.*.dot; do \
        $(MAKE) $${f%.*}.pdf; \
        $(RM) $${f%.*}.dot; \
    done

%.model.dump: %.ph.lp.ce.cc.be.f.json \
              %.presolved.aimaps.json \
              %.presolved.ll.model.json
	$(UNI_IS_CMD) make --dump-low-level-model-info \
                  -f $*.ph.lp.ce.cc.be.f.json \
                  -m $*.presolved.ll.model.json \
                  -a $*.presolved.aimaps.json \
                  -o $@

%.sol.dump: %.ph.lp.ce.cc.be.f.json \
            %.presolved.aimaps.json \
            %.presolved.ll.model.json \
            %.presolved.ll.sol.json
	$(UNI_IS_CMD) make --dump-low-level-solution-info \
                  -f $*.ph.lp.ce.cc.be.f.json \
                  -m $*.presolved.ll.model.json \
                  -a $*.presolved.aimaps.json \
                  -s $*.presolved.ll.sol.json \
                  -o $@



#==============
# MISC TARGETS
#==============

%.run-checks:
	@echo "$(BLUE)* CHECKING INTEGRITY$(NC)"
	@for f in $*.f.json \
			  $*.ph.f.json \
			  $*.ph.lp.f.json \
			  $*.ph.lp.ce.f.json \
			  $*.ph.lp.ce.cc.f.json \
			  $*.ph.lp.ce.cc.be.f.json \
			  ; do \
		echo -n "    $$f " ;\
		if [ -f $$f ]; then \
			out=`mktemp` ;\
			msg=`$(UNI_IS_CMD) check --check-fun-integrity -f $$f \
				 > $$out 2>&1` ;\
			if [ "$$?" -eq "0" ]; then \
				if [ ! -s $$out ]; then \
					echo "[$(GREEN)OK$(NC)]" ;\
				else \
					echo "[$(RED)WARNING$(NC)]" ;\
					cat $$out ;\
				fi \
			else \
				echo "[$(RED)FAILED$(NC)]" ;\
				cat $$out ;\
			fi \
		else \
			echo "[$(YELLOW)IGNORED$(NC)] (not found)" ;\
		fi \
	done
	@echo "$(BLUE)* CHECKING COVERABILITY$(NC)"
	@echo -n "    $*.ph.lp.ce.cc.be.f.json "
	@if [ -f $*.ph.lp.ce.cc.be.f.json ]; then \
		if [ -f $*.ph.lp.ce.cc.be.ae.p.json ]; then \
			out=`mktemp` ;\
			msg=`$(UNI_IS_CMD) check --check-fun-coverage \
			                   -f $*.ph.lp.ce.cc.be.f.json \
			                   -p $*.ph.lp.ce.cc.be.ae.p.json \
							   > $$out 2>&1` ;\
			if [ "$$?" -eq "0" ]; then \
				echo "[$(GREEN)OK$(NC)]" ;\
			else \
				echo "[$(RED)FAILED$(NC)]" ;\
				cat $$out ;\
			fi \
		else \
			echo "[$(YELLOW)IGNORED$(NC)] (has no pattern matchset)" ;\
		fi \
	else \
		echo "[$(YELLOW)IGNORED$(NC)] (not found)" ;\
	fi
	@echo "$(BLUE)* CHECKING OVERLAP OF DATA LOCATIONS$(NC)"
	@echo -n "    $*.ph.lp.ce.cc.be.f.json "
	@if [ -f $*.ph.lp.ce.cc.be.f.json ]; then \
		if [ -f $*.ph.lp.ce.cc.be.ae.p.json ]; then \
			out=`mktemp` ;\
			msg=`$(UNI_IS_CMD) check --check-fun-location-overlap \
			                   -f $*.ph.lp.ce.cc.be.f.json \
			                   -p $*.ph.lp.ce.cc.be.ae.p.json \
							   > $$out 2>&1` ;\
			if [ "$$?" -eq "0" ]; then \
				echo "[$(GREEN)OK$(NC)]" ;\
			else \
				echo "[$(RED)FAILED$(NC)]" ;\
				cat $$out ;\
			fi \
		else \
			echo "[$(YELLOW)IGNORED$(NC)] (has no pattern matchset)" ;\
		fi \
	else \
		echo "[$(YELLOW)IGNORED$(NC)] (not found)" ;\
	fi

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

regenerate-targets:
	for json in *.arch.json; do \
		$(UNI_TARGEN_CMD) -m $$json -o ../uni-is/UniIS/Targets ;\
	done

fetch-test-cases:
	TARGET_LC=`echo $(TARGET) | tr A-Z a-z` ;\
	cp ../experiments/mediabench/selected-$$TARGET_LC/*.ll .



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
