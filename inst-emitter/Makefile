# Copyright (c) 2014, Gabriel Hjort Blindell <ghb@kth.se>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.



#===================
# PROGRAM VARIABLES
#===================

BINARY   := inst-emitter
HLIBPATH := ../hlib



#===================
# COMPILATION FLAGS
#===================

GHC      := ghc
GHCFLAGS += -Wall -i$(HLIBPATH)



#====================
# INTERNAL FUNCTIONS
#====================

# Recursively removes all *.hi and *.o files within the given path argument.
#
# Arguments:
#    1: Path to folder.
define ghc-clean
  find $(1) -name "*.hi" -exec rm {} \;
  find $(1) -name "*.o" -exec rm {} \;
endef



#=======
# RULES
#=======

.PHONY: all
all: $(BINARY)

.PHONY: $(BINARY)
$(BINARY):
	$(GHC) $(GHCFLAGS) -o $@ InstEmitter.hs

.PHONY: clean
clean:
	$(call ghc-clean,$(HLIBPATH))
	$(call ghc-clean,.)

.PHONY: distclean
distclean: clean
	-$(RM) $(BINARY)
