#
#  Main authors:
#    Gabriel Hjort Blindell <ghb@kth.se>
#
#  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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



#==========================
# EXTERNALLY SET VARIABLES
#==========================

# Should be set and exported as an environment variable
LLVM_INT_IS_BUILD_DIR ?= @echo 'ERROR: Environment variable' \
                               '$$LLVM_INT_IS_BUILD_DIR not set!' ; \
                         exit 1 ;

# Should be set from within the Makefile
UNI_IS_CMD            ?= @echo 'ERROR: Variable $$UNI_IS_CMD not set!' ; \
                          exit 1 ;
UNI_IS_LLVM_CMD       ?= @echo 'ERROR: Variable $$UNI_IS_LLVM_CMD not set!' ; \
                          exit 1 ;
CONSTR_CONV_CMD       ?= @echo 'ERROR: Variable $$CONSTR_CONV_CMD not set!' ; \
                          exit 1 ;
DOM_MATCHES_CMD       ?= @echo 'ERROR: Variable $$DOM_MATCHES_CMD not set!' ; \
                          exit 1 ;
ILL_MATCHES_CMD       ?= @echo 'ERROR: Variable $$ILL_MATCHES_CMD not set!' ; \
                          exit 1 ;
PRUNE_BAD_MATCHES_CMD ?= @echo 'ERROR: Variable $$PRUNE_BAD_MATCHES_CMD' \
                               'not set!' ; \
                          exit 1 ;
SOLVER_CMD            ?= @echo 'ERROR: Variable $$SOLVER_CMD not set!' ; \
                          exit 1 ;
ALT_LIMIT             ?= # 0 indicates no limit, 1 indicates no inserts
SOLVER_TIME_LIMIT     ?= # In seconds; 0 indicates no timelimit
DISABLE_UPPER_BOUND   ?= 0 # 1 disables use of upper bound
FALL_BACK_TO_LLVM     ?= 0 # 1 enables use of LLVM to always provide a solution
TARGET                ?=
LLC_TARGET_FLAGS      ?=

GET_JSON_FIELD        ?= @echo 'ERROR: Variable $$GET_JSON_FIELD' \
                               'not set!' ; \



#==========================
# INTERNALLY SET VARIABLES
#==========================

OPT            := $(LLVM_INT_IS_BUILD_DIR)/bin/opt
LLC            := $(LLVM_INT_IS_BUILD_DIR)/bin/llc
LCLIB          := $(LLVM_INT_IS_BUILD_DIR)/lib/LibLiftConstExprs.so
LSLIB          := $(LLVM_INT_IS_BUILD_DIR)/lib/LibLowerSelect.so
LGLIB          := $(LLVM_INT_IS_BUILD_DIR)/lib/LibLowerGetElementPtr.so
AEFMLIB        := $(LLVM_INT_IS_BUILD_DIR)/lib/LibAttachExecFreqMetadata.so
LLC_ISEL_FLAGS := -O0 $(LLC_TARGET_FLAGS) -fast-isel=false



#=================
# TOOLCHAIN RULES
#=================

%.low.ll: %.ll
	$(OPT) -lowerswitch \
		   -load $(LCLIB) -lift-const-exprs \
		   -load $(LSLIB) -lowerselect \
		   -load $(LGLIB) -lowergetelementptr \
		   -S $< -o $@

%.low.freq.ll: %.low.ll
	$(OPT) -load $(AEFMLIB) -attach-exec-freq-metadata -S $< -o $@

%.llvm.json: %.low.freq.ll
	$(LLC) $(LLC_ISEL_FLAGS) -print-isel-cost $< -o /dev/null > $@ 2> /dev/null

%.ub.json: %.llvm.json
	if [ $(DISABLE_UPPER_BOUND) -eq 0 ]; then \
		$(GET_JSON_FIELD) "$<" cycles > $@; \
	else \
		echo "0" > $@; \
	fi

%.f.json: %.low.freq.ll
	$(UNI_IS_LLVM_CMD) make --construct-fun-from-llvm -f $< -o $@

%.lp.f.json: %.f.json
	$(UNI_IS_CMD) transform --lower-pointers-in-fun -f $< -t $(TARGET) -o $@

%.ep.f.json: %.f.json
	$(UNI_IS_CMD) transform \
				  --enforce-phi-invariants-in-fun \
				  -f $< \
				  -t $(TARGET) \
				  -o $@

%.de.f.json: %.f.json
	$(UNI_IS_CMD) transform --remove-dead-code-in-fun -f $< -o $@

%.de.lp.ep.ce.cc.rp.de.f.json: %.de.lp.ep.ce.cc.rp.f.json
	$(UNI_IS_CMD) transform --remove-dead-code-in-fun -f $< -o $@

%.ce.f.json: %.f.json
	$(UNI_IS_CMD) transform --copy-extend-fun -f $< -o $@

%.cc.f.json: %.f.json
	$(UNI_IS_CMD) transform --combine-consts-in-fun -f $< -o $@

%.rp.f.json: %.f.json
	$(UNI_IS_CMD) transform --remove-phi-redundancies-in-fun -f $< -o $@

%.rt.f.json: %.f.json
	$(UNI_IS_CMD) transform --remove-conv-redundancies-in-fun -f $< -o $@

%.p.json: %.f.json
	$(UNI_IS_CMD) make --compute-pattern-matchset -t $(TARGET) -f $< -o $@

%.ae.p.json: %.f.json %.p.json
	$(UNI_IS_CMD) transform \
				  --alternative-extend-pat \
				  --alt-limit $(ALT_LIMIT) \
				  -f $*.f.json \
				  -p $*.p.json \
				  -o $@

%.de.lp.ep.ce.cc.rp.de.rt.dom.json: %.ll.model.json
	$(CONSTR_CONV_CMD) $< > $<.temp
	$(DOM_MATCHES_CMD) $<.temp > $@
	$(RM) $<.temp

%.de.lp.ep.ce.cc.rp.de.rt.ill.json: %.ll.model.json
	$(ILL_MATCHES_CMD) $< > $@

%.de.lp.ep.ce.cc.rp.de.rt.ae.presolved.p.json: \
        %.de.lp.ep.ce.cc.rp.de.rt.ae.p.json \
        %.de.lp.ep.ce.cc.rp.de.rt.dom.json \
        %.de.lp.ep.ce.cc.rp.de.rt.ill.json \
        %.aimaps.json
	$(PRUNE_BAD_MATCHES_CMD) -d $*.de.lp.ep.ce.cc.rp.de.rt.dom.json \
							 -i $*.de.lp.ep.ce.cc.rp.de.rt.ill.json \
							 -p $*.de.lp.ep.ce.cc.rp.de.rt.ae.p.json \
							 -a $*.aimaps.json \
							 > $@

%.presolved.hl.model.json: %.de.lp.ep.ce.cc.rp.de.rt.f.json \
                           %.de.lp.ep.ce.cc.rp.de.rt.ae.presolved.p.json
	$(UNI_IS_CMD) make --construct-hl-cp-model \
				  -f $*.de.lp.ep.ce.cc.rp.de.rt.f.json \
				  -p $*.de.lp.ep.ce.cc.rp.de.rt.ae.presolved.p.json \
				  -o $@

%.hl.model.json: %.de.lp.ep.ce.cc.rp.de.rt.f.json \
                 %.de.lp.ep.ce.cc.rp.de.rt.ae.p.json
	$(UNI_IS_CMD) make --construct-hl-cp-model \
				  -f $*.de.lp.ep.ce.cc.rp.de.rt.f.json \
				  -p $*.de.lp.ep.ce.cc.rp.de.rt.ae.p.json \
				  -o $@

%.presolved.aimaps.json: %.de.lp.ep.ce.cc.rp.de.rt.f.json \
                         %.presolved.hl.model.json
	$(UNI_IS_CMD) make --compute-array-index-maplists \
				  -f $*.de.lp.ep.ce.cc.rp.de.rt.f.json \
				  -m $*.presolved.hl.model.json \
				  -o $@

%.aimaps.json: %.de.lp.ep.ce.cc.rp.de.rt.f.json %.hl.model.json
	$(UNI_IS_CMD) make --compute-array-index-maplists \
				  -f $*.de.lp.ep.ce.cc.rp.de.rt.f.json \
				  -m $*.hl.model.json \
				  -o $@

%.ll.model.json: %.hl.model.json %.aimaps.json
	$(UNI_IS_CMD) transform --lower-hl-cp-model \
				  -m $*.hl.model.json \
				  -a $*.aimaps.json \
				  -o $@

%.presolved.ll.sol.json: %.presolved.ll.model.json %.ub.json
	$(SOLVER_CMD) -i $*.presolved.ll.model.json \
				  -t $(SOLVER_TIME_LIMIT) \
				  -u $(shell cat $*.ub.json) \
				  -f $(FALL_BACK_TO_LLVM) \
				  -o $@

%.presolved.hl.sol.json: %.presolved.hl.model.json \
                         %.presolved.ll.sol.json %.aimaps.json
	$(UNI_IS_CMD) transform --raise-ll-cp-solution \
				  -m $*.presolved.hl.model.json \
				  -s $*.presolved.ll.sol.json \
				  -a $*.presolved.aimaps.json \
				  -o $@

%.s: %.ll %.presolved.hl.model.json %.presolved.hl.sol.json
	HAS_SOL=`$(GET_JSON_FIELD) $*.presolved.hl.sol.json "has-solution" | \
			 tr '[:upper:]' '[:lower:]'`; \
	if [ "$$HAS_SOL" = "true" ]; then \
		$(UNI_IS_CMD) make --generate-asm \
					  -m $*.presolved.hl.model.json \
					  -s $*.presolved.hl.sol.json \
					  -o $@; \
	else \
		$(LLC) $(LLC_ISEL_FLAGS) -dump-isel-w-costs -disable-combiner \
			$*.low.freq.ll -o /dev/null > $@; \
	fi
