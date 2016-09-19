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
UNI_IS_LLVM_BUILD_DIR ?= @echo 'ERROR: Environment variable' \
                               '$$UNI_IS_LLVM_BUILD_DIR not set!' ; \
                         exit 1 ;

# Should be set from within the Makefile
UNI_IS_CMD            ?= @echo 'ERROR: Variable $$UNI_IS_CMD not set!' ; \
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
SOLVER_TIMELIMIT      ?= # In seconds; 0 indicates no timelimit
TARGET                ?=



#==========================
# INTERNALLY SET VARIABLES
#==========================

CLANG          := $(UNI_IS_LLVM_BUILD_DIR)/bin/clang
OPT            := $(UNI_IS_LLVM_BUILD_DIR)/bin/opt
LSLIB          := $(UNI_IS_LLVM_BUILD_DIR)/lib/LibLowerSelect.so
AEFMLIB        := $(UNI_IS_LLVM_BUILD_DIR)/lib/LibAttachExecFreqMetadata.so



#=================
# TOOLCHAIN RULES
#=================

%.ll: %.c
	$(CLANG) -emit-llvm -S $< -o $@

%.low.ll: %.ll
	$(OPT) -load $(LSLIB) -mem2reg -lowerselect -lowerswitch -S $< -o $@

%.low.freq.ll: %.low.ll
	$(OPT) -load $(AEFMLIB) -attach-exec-freq-metadata -S $< -o $@

%.f.json: %.low.freq.ll
	$(UNI_IS_CMD) make --construct-fun-from-llvm -f $< -o $@

%.ce.f.json: %.f.json
	$(UNI_IS_CMD) transform --copy-extend-fun -f $< -o $@

%.ce.cc.f.json: %.ce.f.json
	$(UNI_IS_CMD) transform --combine-consts-in-fun -f $< -o $@

%.ce.cc.ae.f.json: %.ce.cc.f.json
	$(UNI_IS_CMD) transform --alternative-extend-fun -f $< -o $@

%.ce.cc.ae.be.f.json: %.ce.cc.ae.f.json
	$(UNI_IS_CMD) transform --branch-extend-fun -f $< -o $@

%.p.json: %.f.json
	$(UNI_IS_CMD) make --compute-pattern-matchset -t $(TARGET) -f $< -o $@

%.ce.cc.ae.be.dom.json: %.ll.model.json
	$(CONSTR_CONV_CMD) $< > $@.temp
	$(DOM_MATCHES_CMD) $@.temp > $@
	$(RM) $@.temp

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
	$(UNI_IS_CMD) make --construct-hl-cp-model-no-op \
                  -f $*.ce.cc.ae.be.f.json \
                  -p $*.ce.cc.ae.be.presolved.p.json \
                  -o $@

%.hl.model-no-op.json: %.ce.cc.ae.be.f.json %.ce.cc.ae.be.p.json
	$(UNI_IS_CMD) make --construct-hl-cp-model-no-op \
                  -f $*.ce.cc.ae.be.f.json \
                  -p $*.ce.cc.ae.be.p.json \
                  -o $@

%.presolved.hl.model-w-op.json: %.presolved.hl.model-no-op.json \
                                %.ce.cc.ae.be.presolved.p.json
	$(UNI_IS_CMD) make --construct-hl-cp-model-w-op \
                  -m $*.presolved.hl.model-no-op.json \
                  -p $*.ce.cc.ae.be.presolved.p.json \
                  -o $@

%.hl.model-w-op.json: %.hl.model-no-op.json %.ce.cc.ae.be.p.json
	$(UNI_IS_CMD) make --construct-hl-cp-model-w-op \
               -m $*.hl.model-no-op.json \
               -p $*.ce.cc.ae.be.p.json \
               -o $@

%.presolved.aimaps.json: %.ce.cc.ae.be.f.json %.presolved.hl.model-w-op.json
	$(UNI_IS_CMD) make --compute-array-index-maplists \
                  -f $*.ce.cc.ae.be.f.json \
                  -m $*.presolved.hl.model-w-op.json \
                  -o $@

%.aimaps.json: %.ce.cc.ae.be.f.json %.hl.model-w-op.json
	$(UNI_IS_CMD) make --compute-array-index-maplists \
                  -f $*.ce.cc.ae.be.f.json \
                  -m $*.hl.model-w-op.json \
                  -o $@

%.ll.model.json: %.hl.model-w-op.json %.aimaps.json
	$(UNI_IS_CMD) transform --lower-hl-cp-model \
                  -m $*.hl.model-w-op.json \
                  -a $*.aimaps.json \
                  -o $@

%.ll.sol.json: %.ll.model.json
	$(SOLVER_CMD) -t $(SOLVER_TIMELIMIT) -o $@ $<

%.hl.sol.json: %.hl.model-w-op.json %.ll.sol.json %.aimaps.json
	$(UNI_IS_CMD) transform --raise-ll-cp-solution \
                  -m $*.hl.model-w-op.json \
                  -s $*.ll.sol.json \
                  -a $*.aimaps.json \
                  -o $@

%.s: %.presolved.hl.model-w-op.json %.presolved.hl.sol.json
	$(UNI_IS_CMD) make --generate-asm \
                  -m $*.presolved.hl.model-w-op.json \
                  -s $*.presolved.hl.sol.json \
                  -o $@
