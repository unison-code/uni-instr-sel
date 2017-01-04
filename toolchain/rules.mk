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
ALT_INSERT_LIMIT      ?= # 0 indicates no timelimit
SOLVER_TIME_LIMIT     ?= # In seconds; 0 indicates no timelimit
TARGET                ?=



#==========================
# INTERNALLY SET VARIABLES
#==========================

OPT            := $(LLVM_INT_IS_BUILD_DIR)/bin/opt
LCLIB          := $(LLVM_INT_IS_BUILD_DIR)/lib/LibLiftConstExprs.so
LSLIB          := $(LLVM_INT_IS_BUILD_DIR)/lib/LibLowerSelect.so
LGLIB          := $(LLVM_INT_IS_BUILD_DIR)/lib/LibLowerGetElementPtr.so
AEFMLIB        := $(LLVM_INT_IS_BUILD_DIR)/lib/LibAttachExecFreqMetadata.so



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

%.f.json: %.low.freq.ll
	$(UNI_IS_LLVM_CMD) make --construct-fun-from-llvm -f $< -o $@

%.lp.f.json: %.f.json
	$(UNI_IS_CMD) transform --lower-pointers-in-fun -f $< -t $(TARGET) -o $@

%.lp.ph.f.json: %.lp.f.json
	$(UNI_IS_CMD) transform \
				  --enforce-phi-invariants-in-fun \
				  -f $< \
				  -t $(TARGET) \
				  -o $@

%.lp.ph.ce.f.json: %.lp.ph.f.json
	$(UNI_IS_CMD) transform --copy-extend-fun -f $< -o $@

%.lp.ph.ce.cc.f.json: %.lp.ph.ce.f.json
	$(UNI_IS_CMD) transform --combine-consts-in-fun -f $< -o $@

%.lp.ph.ce.cc.ph.f.json: %.lp.ph.ce.cc.f.json
	$(UNI_IS_CMD) transform --remove-phi-redundancies-in-fun -f $< -o $@

%.lp.ph.ce.cc.ph.be.f.json: %.lp.ph.ce.cc.ph.f.json
	$(UNI_IS_CMD) transform --branch-extend-fun -f $< -o $@

%.p.json: %.f.json
	$(UNI_IS_CMD) make --compute-pattern-matchset -t $(TARGET) -f $< -o $@

%.ae.p.json: %.f.json %.p.json
	$(UNI_IS_CMD) transform \
				  --alternative-extend-pat \
				  --alt-insert-limit $(ALT_INSERT_LIMIT) \
				  -f $*.f.json \
				  -p $*.p.json \
				  -o $@

%.lp.ph.ce.cc.ph.be.dom.json: %.ll.model.json
	$(CONSTR_CONV_CMD) $< > $@.temp
	$(DOM_MATCHES_CMD) $@.temp > $@
	$(RM) $@.temp

%.lp.ph.ce.cc.ph.be.ill.json: %.ll.model.json
	$(ILL_MATCHES_CMD) $< > $@

%.lp.ph.ce.cc.ph.be.ae.presolved.p.json: %.lp.ph.ce.cc.ph.be.ae.p.json \
                                         %.lp.ph.ce.cc.ph.be.dom.json \
                                         %.lp.ph.ce.cc.ph.be.ill.json \
                                         %.aimaps.json
	$(PRUNE_BAD_MATCHES_CMD) -d $*.lp.ph.ce.cc.ph.be.dom.json \
							 -i $*.lp.ph.ce.cc.ph.be.ill.json \
							 -p $*.lp.ph.ce.cc.ph.be.ae.p.json \
							 -a $*.aimaps.json \
							 > $@

%.presolved.hl.model.json: %.lp.ph.ce.cc.ph.be.f.json \
                           %.lp.ph.ce.cc.ph.be.ae.presolved.p.json
	$(UNI_IS_CMD) make --construct-hl-cp-model \
				  -f $*.lp.ph.ce.cc.ph.be.f.json \
				  -p $*.lp.ph.ce.cc.ph.be.ae.presolved.p.json \
				  -o $@

%.hl.model.json: %.lp.ph.ce.cc.ph.be.f.json %.lp.ph.ce.cc.ph.be.ae.p.json
	$(UNI_IS_CMD) make --construct-hl-cp-model \
				  -f $*.lp.ph.ce.cc.ph.be.f.json \
				  -p $*.lp.ph.ce.cc.ph.be.ae.p.json \
				  -o $@

%.presolved.aimaps.json: %.lp.ph.ce.cc.ph.be.f.json %.presolved.hl.model.json
	$(UNI_IS_CMD) make --compute-array-index-maplists \
				  -f $*.lp.ph.ce.cc.ph.be.f.json \
				  -m $*.presolved.hl.model.json \
				  -o $@

%.aimaps.json: %.lp.ph.ce.cc.ph.be.f.json %.hl.model.json
	$(UNI_IS_CMD) make --compute-array-index-maplists \
				  -f $*.lp.ph.ce.cc.ph.be.f.json \
				  -m $*.hl.model.json \
				  -o $@

%.ll.model.json: %.hl.model.json %.aimaps.json
	$(UNI_IS_CMD) transform --lower-hl-cp-model \
				  -m $*.hl.model.json \
				  -a $*.aimaps.json \
				  -o $@

%.ll.sol.json: %.ll.model.json
	$(SOLVER_CMD) -t $(SOLVER_TIME_LIMIT) -o $@ $<

%.hl.sol.json: %.hl.model.json %.ll.sol.json %.aimaps.json
	$(UNI_IS_CMD) transform --raise-ll-cp-solution \
				  -m $*.hl.model.json \
				  -s $*.ll.sol.json \
				  -a $*.aimaps.json \
				  -o $@

%.s: %.presolved.hl.model.json %.presolved.hl.sol.json
	$(UNI_IS_CMD) make --generate-asm \
				  -m $*.presolved.hl.model.json \
				  -s $*.presolved.hl.sol.json \
				  -o $@
