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
MINIZINC_BACKEND_FLAGS := -f

CLANG                 := $(LLVM_INT_IS_BUILD_DIR)/bin/clang
UNI_IS_LLVM_CMD       := uni-is-llvm
UNI_IS_CMD            := uni-is
UNI_TARGEN_CMD        := uni-targen
CONSTR_CONV_CMD       := ../solvers/presolver/constraints-converter
DOM_MATCHES_CMD       := ../solvers/presolver/find-dominated-matches
ILL_MATCHES_CMD       := ../solvers/presolver/find-illegal-matches
PRUNE_BAD_MATCHES_CMD := ../solvers/presolver/prune-bad-matches
SOLVER_CMD            := ../solvers/minizinc/solver \
                         -g $(MINIZINC_GLOBALS_DIR) \
                         -s "$(MINIZINC_BACKEND) $(MINIZINC_BACKEND_FLAGS)"
ALT_INSERT_LIMIT      := 3 # 0 indicates no limit
SOLVER_TIME_LIMIT     := 0 # In seconds; 0 indicates no timelimit

TARGET := mips32

REGRESSION_TESTS_DIR := ./regression

# Bold green
C_OK=\033[1;32m
# Bold red
C_ERROR=\033[1;31m
# Normal red
C_WARNING=\033[0;31m
# Bold yellow
C_IGNORED=\033[1;33m
# Bold blue
C_INFO=\033[1;34m
# No color
C_NONE=\033[0m




#=============
# BUILD RULES
#=============

.PHONY: build
build:
	cd .. && make build

.PHONY: hlib
hlib:
	cd .. && make hlib

.PHONY: uni-targen
uni-targen:
	cd .. && make uni-targen

.PHONY: uni-is-llvm
uni-is-llvm:
	cd .. && make uni-is-llvm

.PHONY: uni-is
uni-is:
	cd .. && make uni-is

.PHONY: uni-is-prof
uni-is-prof:
	cd .. && make uni-is-prof

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

.PHONY: run-all-regression-tests
run-all-regression-tests:
	@$(MAKE) --no-print-directory run-small-regression-tests
	@$(MAKE) --no-print-directory run-medium-regression-tests

.PHONY: run-small-regression-tests
run-small-regression-tests:
	@$(MAKE) --no-print-directory T_ID=SMALL T_DB=small-test-cases.db \
			 .run-regression-tests-for-all-targets

.PHONY: run-medium-regression-tests
run-medium-regression-tests:
	@$(MAKE) --no-print-directory T_ID=MEDIUM T_DB=medium-test-cases.db \
			 .run-regression-tests-for-all-targets

.PHONY: .run-regression-tests-for-all-targets
.run-regression-tests-for-all-targets:
	@$(MAKE) TARGET=Mips32   .run-regression-tests
	@$(MAKE) TARGET=Hexagon5 .run-regression-tests

.PHONY: .run-regression-tests
.run-regression-tests:
	@echo
	@echo -n "$(C_INFO)"
	@echo "=========================================================="
	@echo "=   $(T_ID) REGRESSION TESTS FOR TARGET: $(C_NONE)$(TARGET)$(C_INFO)"
	@echo "=========================================================="
	@echo -n "$(C_NONE)"
	@echo
	@echo "$(C_INFO)NOTE: Solving time is given in parentheses$(C_NONE)"
	@echo
	@echo "$(C_INFO)* EXECUTING REGRESSION TESTS$:$(C_NONE)"
	@\
	T_INDENT="    " ;\
	T_TOTAL=0 ;\
	T_PASSED=0 ;\
	T_IGNORED=0 ;\
	T_FAILED=0 ;\
	T_WARN=0 ;\
	T_ALL=`cat $(REGRESSION_TESTS_DIR)/$(T_DB)` ;\
	T_MAX_LENGTH=0 ;\
	T_TIME_START=`date +%s` ;\
	T_PMAT_TIME_TOTAL=0 ;\
	T_PREP_TIME_TOTAL=0 ;\
	T_SOLV_TIME_TOTAL=0 ;\
	for T in $$T_ALL; do \
		if [ "$${#T}" -gt "$$T_MAX_LENGTH" ]; then \
			T_MAX_LENGTH=$${#T} ;\
	    fi \
    done ;\
	for T in $$T_ALL; do \
	    T_TOTAL=`expr $$T_TOTAL + 1` ;\
		echo -n "$$T_INDENT$$T" ;\
		for i in `seq $${#T} $$T_MAX_LENGTH`; do \
			echo -n " " ;\
		done ;\
		TARGET_LC=`echo $(TARGET) | tr A-Z a-z` ;\
		RT_TARGET_DIR=$(REGRESSION_TESTS_DIR)/$$TARGET_LC ;\
		T_COST=`cat $$RT_TARGET_DIR/$$T.cost 2> /dev/null` ;\
		T_ORACLES=`ls $$RT_TARGET_DIR/$$T.s.* 2> /dev/null` ;\
		if [ -n "$$T_COST" ]; then \
			if [ -n "$$T_ORACLES" ]; then \
				TEMPFILE=$$(basename `mktemp`) ;\
				T_LL=$${TEMPFILE}.ll ;\
			    T_ASM=$${TEMPFILE}.s ;\
				cp $(REGRESSION_TESTS_DIR)/$$T.ll $$T_LL ;\
				$(MAKE) $$T_ASM > /dev/null 2>&1 ;\
			    T_ASM_TXT=`cat $$T_ASM 2> /dev/null` ;\
				subfix=".de.lp.ep.ce.cc.rp.de.be.ae.presolved.p.json" ;\
				pmatch_file="$${T_LL%.*}$$subfix" ;\
				if [ -f $$pmatch_file ]; then \
					pmatch_time=`$(REGRESSION_TESTS_DIR)/get-json-field \
								 "$$pmatch_file" "time" 2> /dev/null` ;\
				else \
					pmatch_time=0 ;\
				fi ;\
				T_PMAT_TIME_TOTAL=`echo "scale=2; \
							 			 t = $$T_PMAT_TIME_TOTAL + \
										 	   $$pmatch_time; \
										 if (t < 1) print 0; t" \
								   | bc` ;\
				sol_file="$${T_LL%.*}.presolved.hl.sol.json" ;\
				if [ -f $$sol_file ]; then \
					cost=`$(REGRESSION_TESTS_DIR)/get-json-field \
						  "$$sol_file" "cost" 2> /dev/null` ;\
					solv_time=`$(REGRESSION_TESTS_DIR)/get-json-field \
							   "$$sol_file" "solving-time" 2> /dev/null` ;\
					T_SOLV_TIME_TOTAL=`echo "scale=2; \
								 			 t = $$T_SOLV_TIME_TOTAL + \
												 $$solv_time; \
											 if (t < 1) print 0; t" \
									   | bc` ;\
					prep_time=`$(REGRESSION_TESTS_DIR)/get-json-field \
							   "$$sol_file" "prep-time" 2> /dev/null` ;\
					T_PREP_TIME_TOTAL=`echo "scale=2; \
								 			 t = $$T_PREP_TIME_TOTAL + \
												 $$prep_time; \
											 if (t < 1) print 0; t" \
									   | bc` ;\
				else \
					cost="-1" ;\
					solv_time="?" ;\
				fi ;\
				if [ -f "$$T_ASM" ] && [ -n "$$T_ASM_TXT" ]; then \
					if [ "$$cost" -eq "$$T_COST" ]; then \
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
							echo "[$(C_OK)OK$(C_NONE)] ($${solv_time}s)" ;\
						else \
							T_WARN=`expr $$T_WARN + 1` ;\
							echo -n "[$(C_WARNING)WARNING$(C_NONE)] " ;\
							echo -n "($${solv_time}s) " ;\
							echo    "(diff output)" ;\
							for i in `seq 1 58`; do \
								echo -n "-" ;\
							done ;\
						    echo ;\
							echo "- DIFF -";\
							echo "$$T_DIFF" ;\
						    echo ;\
							echo "- FULL OUTPUT -";\
							cat $$T_ASM ;\
							for i in `seq 1 58`; do \
								echo -n "-" ;\
							done ;\
							echo ;\
						fi \
					else \
						T_FAILED=`expr $$T_FAILED + 1` ;\
						echo -n "[$(C_ERROR)FAILED$(C_NONE)] " ;\
						echo -n "($${solv_time}s) " ;\
						echo    "(diff costs)" ;\
						for i in `seq 0 $$T_MAX_LENGTH`; do \
							echo -n " " ;\
						done ;\
						echo "$${T_INDENT}Expected: $$T_COST" ;\
						for i in `seq 0 $$T_MAX_LENGTH`; do \
							echo -n " " ;\
						done ;\
						echo "$${T_INDENT}Actual: $$cost" ;\
					fi ;\
				else \
					T_FAILED=`expr $$T_FAILED + 1` ;\
					echo -n "[$(C_ERROR)FAILED$(C_NONE)] " ;\
					echo -n "($${solv_time}s) " ;\
					echo    "(no output)" ;\
				fi ;\
				rm -f $${TEMPFILE}.* ;\
			else \
			    T_IGNORED=`expr $$T_IGNORED + 1` ;\
				echo "[$(C_IGNORED)IGNORED$(C_NONE)] (has no oracles)" ;\
			fi ;\
		else \
		    T_IGNORED=`expr $$T_IGNORED + 1` ;\
			echo "[$(C_IGNORED)IGNORED$(C_NONE)] (has no cost)" ;\
		fi ;\
	done ;\
	T_TIME_END=`date +%s` ;\
	T_TIME_TOTAL=`expr $$T_TIME_END - $$T_TIME_START` ;\
	T_TIME_TOTAL_PRETTY=`date -u -d @"$$T_TIME_TOTAL" +'%-Mm %-Ss'` ;\
	T_PMAT_TIME_TOTAL=`LC_ALL=en_US.UTF-8 printf '%.*f' 0 \
					   $$T_PMAT_TIME_TOTAL` ;\
	T_PMAT_TIME_TOTAL_PRETTY=`date -u -d @"$$T_PMAT_TIME_TOTAL" +'%-Mm %-Ss'` ;\
	T_PREP_TIME_TOTAL=`LC_ALL=en_US.UTF-8 printf '%.*f' 0 \
					   $$T_PREP_TIME_TOTAL` ;\
	T_PREP_TIME_TOTAL_PRETTY=`date -u -d @"$$T_PREP_TIME_TOTAL" +'%-Mm %-Ss'` ;\
	T_SOLV_TIME_TOTAL=`LC_ALL=en_US.UTF-8 printf '%.*f' 0 \
					   $$T_SOLV_TIME_TOTAL` ;\
	T_SOLV_TIME_TOTAL_PRETTY=`date -u -d @"$$T_SOLV_TIME_TOTAL" +'%-Mm %-Ss'` ;\
	T_MISC_TIME_TOTAL=`expr $$T_TIME_TOTAL - \
					   $$T_SOLV_TIME_TOTAL - \
					   $$T_PREP_TIME_TOTAL - \
					   $$T_PMAT_TIME_TOTAL` ;\
	T_MISC_TIME_TOTAL_PRETTY=`date -u -d @"$$T_MISC_TIME_TOTAL" +'%-Mm %-Ss'` ;\
	if [ "$$T_TIME_TOTAL" -gt "0" ]; then \
		T_PMAT_TIME_PCENT=`expr $$T_PMAT_TIME_TOTAL \* 100 / $$T_TIME_TOTAL` ;\
		T_PREP_TIME_PCENT=`expr $$T_PREP_TIME_TOTAL \* 100 / $$T_TIME_TOTAL` ;\
		T_SOLV_TIME_PCENT=`expr $$T_SOLV_TIME_TOTAL \* 100 / $$T_TIME_TOTAL` ;\
		T_MISC_TIME_PCENT=`expr $$T_MISC_TIME_TOTAL \* 100 / $$T_TIME_TOTAL` ;\
	else \
		T_PMAT_TIME_PCENT=0 ;\
		T_PREP_TIME_PCENT=0 ;\
		T_SOLV_TIME_PCENT=0 ;\
		T_MISC_TIME_PCENT=0 ;\
	fi ;\
	echo ;\
	echo -n "$(C_INFO)" ;\
	echo "==========================================================" ;\
	echo -n "=   PASSED $(C_OK)$$T_PASSED$(C_INFO)" ;\
	if [ "$$T_IGNORED" -ne "0" ]; then \
		echo -n ", IGNORED $(C_IGNORED)$$T_IGNORED$(C_INFO)" ;\
	fi ;\
	if [ "$$T_WARN" -ne "0" ]; then \
		echo -n ", WARNED $(C_WARNING)$$T_WARN$(C_INFO)" ;\
	fi ;\
	if [ "$$T_FAILED" -ne "0" ]; then \
		echo -n ", FAILED $(C_ERROR)$$T_FAILED$(C_INFO)" ;\
	fi ;\
	echo " of $(C_NONE)$$T_TOTAL$(C_INFO) TESTS" ;\
	echo "=" ;\
	echo -n "=   TOTAL TIME:              " ;\
	echo    "$(C_NONE)$${T_TIME_TOTAL_PRETTY}$(C_INFO)" ;\
	echo -n "=   PATTERN MATCHING:        " ;\
	echo -n      "$(C_NONE)$${T_PMAT_TIME_TOTAL_PRETTY} " ;\
	echo         "($${T_PMAT_TIME_PCENT}%)$(C_INFO)" ;\
	echo -n "=   MODEL PREP (FLATTENING): " ;\
	echo -n      "$(C_NONE)$${T_PREP_TIME_TOTAL_PRETTY} " ;\
	echo         "($${T_PREP_TIME_PCENT}%)$(C_INFO)" ;\
	echo -n "=   SOLVING:                 " ;\
	echo -n      "$(C_NONE)$${T_SOLV_TIME_TOTAL_PRETTY} " ;\
	echo         "($${T_SOLV_TIME_PCENT}%)$(C_INFO)" ;\
	echo -n "=   OTHER TASKS:             " ;\
	echo -n      "$(C_NONE)$${T_MISC_TIME_TOTAL_PRETTY} " ;\
	echo         "($${T_MISC_TIME_PCENT}%)$(C_INFO)" ;\
	echo "==========================================================" ;\
	echo -n "$(C_NONE)" ;\
	echo


#=====================
# PLOT AND DUMP RULES
#=====================

%.f.dot: %.f.json
	$(UNI_IS_CMD) plot --plot-fun-full-graph -f $< -o $@

%.cov.dot: %.de.lp.ep.ce.cc.rp.de.be.f.json \
           %.de.lp.ep.ce.cc.rp.de.be.ae.p.json
	$(UNI_IS_CMD) plot --plot-cover-all-matches --hide-null-instrs \
                  -f $*.de.lp.ep.ce.cc.rp.de.be.f.json \
                  -p $*.de.lp.ep.ce.cc.rp.de.be.ae.p.json \
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

%.all-cover-plots: %.de.lp.ep.ce.cc.rp.de.be.f.json \
                   %.de.lp.ep.ce.cc.rp.de.be.ae.p.json
	$(UNI_IS_CMD) plot --plot-cover-per-match \
                  -f $*.de.lp.ep.ce.cc.rp.de.be.f.json \
                  -p $*.de.lp.ep.ce.cc.rp.de.be.ae.p.json \
                  -o $*.cov.dot
	for f in $*.cov.*.dot; do \
        $(MAKE) $${f%.*}.pdf; \
        $(RM) $${f%.*}.dot; \
    done

%.model.dump: %.de.lp.ep.ce.cc.rp.de.be.f.json \
              %.presolved.aimaps.json \
              %.presolved.ll.model.json
	$(UNI_IS_CMD) make --dump-low-level-model-info \
                  -f $*.de.lp.ep.ce.cc.rp.de.be.f.json \
                  -m $*.presolved.ll.model.json \
                  -a $*.presolved.aimaps.json \
                  -o $@

%.sol.dump: %.de.lp.ep.ce.cc.rp.de.be.f.json \
            %.presolved.aimaps.json \
            %.presolved.ll.model.json \
            %.presolved.ll.sol.json
	$(UNI_IS_CMD) make --dump-low-level-solution-info \
                  -f $*.de.lp.ep.ce.cc.rp.de.be.f.json \
                  -m $*.presolved.ll.model.json \
                  -a $*.presolved.aimaps.json \
                  -s $*.presolved.ll.sol.json \
                  -o $@



#==============
# MISC TARGETS
#==============

%.run-checks:
	@echo "$(C_INFO)* CHECKING INTEGRITY$(C_NONE)"
	@for f in $*.f.json \
			  $*.de.f.json \
			  $*.de.lp.f.json \
			  $*.de.lp.ep.f.json \
			  $*.de.lp.ep.ce.f.json \
			  $*.de.lp.ep.ce.cc.f.json \
			  $*.de.lp.ep.ce.cc.rp.f.json \
			  $*.de.lp.ep.ce.cc.rp.de.f.json \
			  $*.de.lp.ep.ce.cc.rp.de.be.f.json \
			  ; do \
		echo -n "    $$f " ;\
		if [ -f $$f ]; then \
			out=`mktemp` ;\
			msg=`$(UNI_IS_CMD) check --check-fun-integrity -f $$f \
				 > $$out 2>&1` ;\
			if [ "$$?" -eq "0" ]; then \
				if [ ! -s $$out ]; then \
					echo "[$(C_OK)OK$(C_NONE)]" ;\
				else \
					echo "[$(C_WARNING)WARNING$(C_NONE)]" ;\
					cat $$out ;\
					echo ;\
				fi \
			else \
				echo "[$(C_ERROR)FAILED$(C_NONE)]" ;\
				cat $$out ;\
				echo ;\
			fi \
		else \
			echo "[$(C_IGNORED)IGNORED$(C_NONE)] (not found)" ;\
		fi \
	done
	@echo "$(C_INFO)* CHECKING COVERABILITY$(C_NONE)"
	@echo -n "    $*.de.lp.ep.ce.cc.rp.de.be.f.json "
	@if [ -f $*.de.lp.ep.ce.cc.rp.de.be.f.json ]; then \
		if [ -f $*.de.lp.ep.ce.cc.rp.de.be.ae.p.json ]; then \
			out=`mktemp` ;\
			msg=`$(UNI_IS_CMD) check --check-fun-coverage \
			                   -f $*.de.lp.ep.ce.cc.rp.de.be.f.json \
			                   -p $*.de.lp.ep.ce.cc.rp.de.be.ae.p.json \
							   > $$out 2>&1` ;\
			if [ "$$?" -eq "0" ]; then \
				echo "[$(C_OK)OK$(C_NONE)]" ;\
			else \
				echo "[$(C_ERROR)FAILED$(C_NONE)]" ;\
				cat $$out ;\
			fi \
		else \
			echo "[$(C_IGNORED)IGNORED$(C_NONE)] (has no pattern matchset)" ;\
		fi \
	else \
		echo "[$(C_IGNORED)IGNORED$(C_NONE)] (not found)" ;\
	fi
	@echo "$(C_INFO)* CHECKING OVERLAP OF DATA LOCATIONS$(C_NONE)"
	@echo -n "    $*.de.lp.ep.ce.cc.rp.de.be.f.json "
	@if [ -f $*.de.lp.ep.ce.cc.rp.de.be.f.json ]; then \
		if [ -f $*.de.lp.ep.ce.cc.rp.de.be.ae.p.json ]; then \
			out=`mktemp` ;\
			msg=`$(UNI_IS_CMD) check --check-fun-location-overlap \
			                   -f $*.de.lp.ep.ce.cc.rp.de.be.f.json \
			                   -p $*.de.lp.ep.ce.cc.rp.de.be.ae.p.json \
							   > $$out 2>&1` ;\
			if [ "$$?" -eq "0" ]; then \
				echo "[$(C_OK)OK$(C_NONE)]" ;\
			else \
				echo "[$(C_ERROR)FAILED$(C_NONE)]" ;\
				cat $$out ;\
			fi \
		else \
			echo "[$(C_IGNORED)IGNORED$(C_NONE)] (has no pattern matchset)" ;\
		fi \
	else \
		echo "[$(C_IGNORED)IGNORED$(C_NONE)] (not found)" ;\
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
