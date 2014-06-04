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



#==========================
# HOW TO USE THIS MAKEFILE
#==========================

# The PROGRAMS variable contains the program binaries to be built by this
# Makefile, separate by at least one space. The binaries are defined as a path
# relative to this Makefile, which may not contain any spaces (as that would be
# interpreted as two separate paths instead of a single path).
#
# For each program binary, a set of modules must be defined on which it is
# dependent. This is done by defining a MODULES variable for each binary. The
# name of the variable is defined as follows:
#
#    MODULES!<program binary path> = <list of module paths>
#
# where <program binary path> is replaced by the entire path as specified in
# the PROGRAMS variable. For example, if
#
#    PROGRAMS = ../path/_to_/-a-/binary ...
#
# then the MODULES variable for the first binary will be:
#
#    MODULES!../path/_to_/-a-/binary
#
# All character in the path are allowed. However, if the path contains '#', ':',
# or '=', then these must be replaced with underscores ('_') when written in the
# variable names.
#
# The LDFLAGS and LDLIBS may also be extended for each program binary by
# defining LDFLAGS!<program binary path> and LDLIBS!<program binary path>, in
# the same was as the MODULES variables are defined.



#===================
# PROGRAM VARIABLES
#===================

# The paths (including file) to the program binaries to build
PROGRAMS := param-splitter

# Defines the application modules for the prettify-json program
MODULES!param-splitter := \
    ../clib/common/exceptions \
    ../clib/common/json       \
    ../clib/common/model      \
    ../clib/common/utils      \
    .



#===================
# COMPILATION FLAGS
#===================

# Flags common to all program builds
CXXFLAGS += -Wall -std=c++11 -g -I../clib
LDFLAGS  +=
LDLIBS   +=



# ========================  BEGINNING OF GENERIC PART  =========================
# ======================== DO NOT EDIT ANYTHING BELOW! =========================



#====================
# INTERNAL FUNCTIONS
#====================

# Converts a path into a name by replacing the following characters:
#    ':' -> '_'
#    '#' -> '_'
#    '=' -> '_'
# Arguments:
#    1: Path.
make-name-from-path = $(subst =,_,$(subst \#,_,$(subst :,_,$(1))))

# Turns a program path, relative to the root, into a program name.
#
# Arguments:
#    1: Path relative to the root.
make-program-name = $(call make-name-from-path,$(1))

# Turns a program path, relative to the root, into a module name.
#
# Arguments:
#    1: Path relative to the root.
# TODO: use make-name-from-path instead
make-module-name = $(call make-name-from-path,$(1))

# Gets the path of the currently loaded module makefile.
get-this-module-path = $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

# Creates a variable '<module-name>-object-files' which contains the filepaths
# of the object files in the module, and appends the module source file to the
# 'all-source-files' variable.
#
# Arguments:
#    1: Module path.
#    2: List of module source filepaths.
define module-template
  $(call make-module-name,$(1))-object-files := $(2:.cpp=.o)
  all-source-files += $(2)
endef

# Creates the prerequisite rules and target-specific variables for a given
# program.
#
# Arguments:
#    1: Program path.
define program-template
  $(1): $$(foreach mod,$$(MODULES!$(call make-program-name,$(1))),\
                   $$(call $$(call make-module-name,$$(mod))-object-files))
  $(1): LDFLAGS += $(LDFLAGS!$(call make-program-name,$(1)))
  $(1): LDLIBS += $(LDLIBS!$(call make-program-name,$(1)))
endef

# Executes doxygen if a 'dox' file is found within the module.
#
# Arguments:
#    1: Module path.
define make-docs
	if [ -f "$(1)/dox" ]; then \
		cd $(1); \
		doxygen dox; \
	fi;
endef

#====================
# INTERNAL VARIABLES
#====================

all-unique-modules = \
    $(sort $(foreach prog,$(PROGRAMS),\
                     $(MODULES!$(call make-program-name,$(prog)))))
all-unique-source-files = $(sort $(all-source-files))
all-unique-object-files = $(all-unique-source-files:.cpp=.o)
all-unique-dep-files = $(all-unique-source-files:.cpp=.d)



#=======
# RULES
#=======

include $(patsubst %,%/module.mk,$(all-unique-modules))

.PHONY: all
all: $(PROGRAMS)

$(foreach prog,$(PROGRAMS),$(eval $(call program-template,$(prog))))

$(PROGRAMS):
	$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(MAKECMDGOALS),docs)
-include $(all-unique-dep-files)
endif
endif
endif

%.d: %.cpp
	$(CXX) $(CXXFLAGS) -M -MT "$(<:.cpp=.o) $@" $< > $@

.PHONY: clean
clean:
	-$(RM) $(all-unique-object-files)

.PHONY: clean-deps
clean-deps:
	-$(RM) $(all-unique-dep-files)

.PHONY: distclean
distclean: clean clean-deps
	-$(RM) $(PROGRAMS)

.PHONY: docs
docs:
	$(foreach mod,$(all-unique-modules),$(call make-docs,$(mod)))
