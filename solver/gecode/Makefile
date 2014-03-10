# Copyright (c) 2013, Gabriel Hjort Blindell <ghb@kth.se>
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



################################################################################
#
# COMPILATION FLAGS

CPPFLAGS += -Wall -std=c++11
LDFLAGS  +=
LDLIBS   +=
CPP      := g++



################################################################################
#
# APPLICATION-RELATED FILES

# Contains the application modules
MODULES := .
TARGET-BINARY := solver
TARBALL-FILENAME := gecode-solver-src



################################################################################
#
# INTERNAL VARIALBES
# (do not edit!)

# List of source files; will be appended to by each module (it is assumed that
# it only contains .cpp files)
source-files :=

# List of object files to compile and link
object-files = $(source-files:.cpp=.o)

# List of dependencies for each source file
dep-files = $(source-files:.cpp=.d)

include $(patsubst %,%/module.mk,$(MODULES))



################################################################################
#
# RULES

all: $(TARGET-BINARY)

$(TARGET-BINARY): $(object-files)
	$(CPP) $(LDFLAGS) -o $@ $^ $(LDLIBS)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(MAKECMDGOALS),docs)
-include $(dep-files)
endif
endif
endif

define make-depends
$(CPP) $(CPPFLAGS) -M -MT "$(<:.cpp=.o) $@" $< > $@
endef

%.d: %.cpp
	$(make-depends)

.PHONY: clean
clean:
	-$(RM) -f $(object-files)

.PHONY: clean-deps
clean-deps:
	-$(RM) -f $(dep-files)

.PHONY: distclean
distclean: clean clean-deps
	-$(RM) -f $(TARGET-BINARY)

.PHONY: docs
docs:
	doxygen dox

.PHONY: tarball
tarball: all
	$(MAKE) clean clean-deps
	$(MAKE) docs
	tar -cvf $(TARBALL-FILENAME).tar ../$$(basename $$(pwd))
	gzip $(TARBALL-FILENAME).tar
