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



#==========
# SETTINGS
#==========

HLIB_PATH        := hlib/instr-sel
UNI_TARGEN_PATH  := uni-targen
UNI_IS_LLVM_PATH := uni-is-llvm
UNI_IS_PATH      := uni-is
SOLVERS_PATH     := solvers
TOOLS_PATH       := tools
LLVM_GENERAL_PURE_PATH := hlib/llvm-general/llvm-general-pure
LLVM_GENERAL_PURE_NAME := llvm-general-pure-3.8.0.0
LLVM_GENERAL_PATH := hlib/llvm-general/llvm-general
LLVM_GENERAL_NAME := llvm-general-3.8.0.0
CABAL_INST_FLAGS :=
CABAL_PROF_FLAGS := --enable-profiling --profiling-detail=all-functions



#================
# HELP FUNCTIONS
#================

define check_pkg
	ghc-pkg list | grep "$(1)"
endef



#=======
# RULES
#=======

.PHONY: build
build: hlib \
	   uni-targen \
	   uni-is-llvm \
	   uni-is \
	   solvers \
	   tools

.PHONY: build-prof
build-prof: hlib-prof \
			uni-targen \
			uni-is-llvm \
			uni-is-prof \
			solvers \
			tools

.PHONY: docs
docs: llvm-general-pure-doc \
	  llvm-general-doc \
	  hlib-doc \
	  uni-targen-doc
	  uni-is-llvm-doc \
	  uni-is-doc \

.PHONY: llvm-general-pure
llvm-general-pure:
	$(eval RES := $(shell $(call check_pkg,$(LLVM_GENERAL_PURE_NAME))))
	if [ -z "$(RES)" ]; then \
	    cd $(LLVM_GENERAL_PURE_PATH) && \
		cabal install $(CABAL_INST_FLAGS); \
	fi

.PHONY: llvm-general-pure-prof
llvm-general-pure-prof:
	$(eval RES := $(shell $(call check_pkg,$(LLVM_GENERAL_PURE_NAME))))
	if [ -z "$(RES)" ]; then \
	    cd $(LLVM_GENERAL_PURE_PATH) && \
		cabal install $(CABAL_INST_FLAGS) $(CABAL_PROF_FLAGS); \
	fi

.PHONY: llvm-general-pure-doc
llvm-general-pure-doc:
	cd $(LLVM_GENERAL_PURE_PATH) && \
	cabal haddock

.PHONY: llvm-general
llvm-general: llvm-general-pure
	$(eval RES := $(shell $(call check_pkg,$(LLVM_GENERAL_NAME))))
	if [ -z "$(RES)" ]; then \
	    cd $(LLVM_GENERAL_PATH) && \
		cabal install $(CABAL_INST_FLAGS); \
	fi

.PHONY: llvm-general-doc
llvm-general-doc: llvm-general-pure-doc
	cd $(LLVM_GENERAL_PATH) && \
	cabal haddock

.PHONY: hlib
hlib: llvm-general-pure
	cd $(HLIB_PATH) && \
	make CABAL_INST_FLAGS="$(CABAL_INST_FLAGS)" install

.PHONY: hlib-prof
hlib-prof: llvm-general-pure-prof
	cd $(HLIB_PATH) && \
	make CABAL_INST_FLAGS="$(CABAL_INST_FLAGS) $(CABAL_PROF_FLAGS)" install

.PHONY: hlib-doc
hlib-doc:
	cd $(HLIB_PATH) && make docs

.PHONY: uni-targen
uni-targen: llvm-general llvm-general-pure hlib
	cd $(UNI_TARGEN_PATH) && \
	make CABAL_INST_FLAGS="$(CABAL_INST_FLAGS)" install

.PHONY: uni-targen-doc
uni-targen-doc:
	cd $(UNI_TARGEN_PATH) && make docs

.PHONY: uni-is-llvm
uni-is-llvm: llvm-general llvm-general-pure hlib
	cd $(UNI_IS_LLVM_PATH) && \
	make CABAL_INST_FLAGS="$(CABAL_INST_FLAGS)" install

.PHONY: uni-is-llvm-doc
uni-is-llvm-doc:
	cd $(UNI_IS_LLVM_PATH) && make docs

.PHONY: uni-is
uni-is: hlib
	cd $(UNI_IS_PATH) && \
	make CABAL_INST_FLAGS="$(CABAL_INST_FLAGS)" install

.PHONY: uni-is-prof
uni-is-prof: hlib-prof
	cd $(UNI_IS_PATH) && \
	make CABAL_INST_FLAGS="$(CABAL_INST_FLAGS) $(CABAL_PROF_FLAGS)" install


.PHONY: uni-is-doc
uni-is-doc:
	cd $(UNI_IS_PATH) && make docs

.PHONY: solvers
solvers:
	cd $(SOLVERS_PATH) && make
	cd $(TOOLS_PATH) && make

.PHONY: tools
tools:
	cd $(TOOLS_PATH) && make

.PHONY: clean
clean:
	cd $(LLVM_GENERAL_PURE_PATH) && $(RM) -r dist
	cd $(LLVM_GENERAL_PURE) && $(RM) -r dist
	cd $(HLIB_PATH) && make clean
	cd $(UNI_TARGEN_PATH) && make clean
	cd $(UNI_IS_LLVM_PATH) && make clean
	cd $(UNI_IS_PATH) && make clean
	cd $(SOLVERS_PATH) && make clean
	cd $(TOOLS_PATH) && make clean

.PHONY: distclean
distclean:
	cd $(LLVM_GENERAL_PURE_PATH) && $(RM) -r dist
	cd $(LLVM_GENERAL_PURE) && $(RM) -r dist
	cd $(HLIB_PATH) && make distclean
	cd $(UNI_TARGEN_PATH) && make distclean
	cd $(UNI_IS_LLVM_PATH) && make distclean
	cd $(UNI_IS_PATH) && make distclean
	cd $(SOLVERS_PATH) && make distclean
	cd $(TOOLS_PATH) && make distclean
