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

# The paths (including file) to the program binaries to build
PROGRAMS := gecode/solver minizinc/mzn-params-gen tools/pretty-json

# Contains the application modules, per program (remember to replace slashes
# in the program path with dashes)
MODULES_gecode-solver   := common/exceptions \
                           common/json       \
                           common/model      \
                           common/utils      \
                           gecode
MODULES_minizinc-mzn-params-gen := common/exceptions \
                                   common/json       \
                                   common/model      \
                                   common/utils      \
                                   minizinc
MODULES_tools-pretty-json := common/json \
                             tools

# Name of the tarball
TARBALL_FILENAME := solvers-src



#===================
# COMPILATION FLAGS
#===================

# Flags common to all program builds
CXXFLAGS += -Wall -std=c++11
LDFLAGS  +=
LDLIBS   +=

# Flags specific to building the Gecode solver (remember to replace slashes in
# the program path with dashes)
LDFLAGS_gecode-solver  +=
LDLIBS_gecode-solver   +=

# Flags specific to building the Minizinc params generator (remember to replace
# slashes in the program path with dashes)
LDFLAGS_minizinc-mzn-params-gen  +=
LDLIBS_minizinc-mzn-params-gen   +=

# Flags specific to building the pretty-json tool (remember to replace slashes
# in the program path with dashes)
LDFLAGS_tools-pretty-json  +=
LDLIBS_tools-pretty-json   +=



# ========================  BEGINNING OF GENERIC PART  =========================
# ======================== DO NOT EDIT ANYTHING BELOW! =========================


#====================
# INTERNAL FUNCTIONS
#====================

# Makes a program name by taking a program path, relative to the root, and
# replaces all slashes (`/') with dashes (`-').
#
# Arguments:
#    1: Path relative to the root.
make-program-name = $(subst /,-,$(1))

# Makes a module name by taking a module path, relative to the root, and
# replaces all slashes (`/') with dashes (`-').
#
# Arguments:
#    1: Path relative to the root.
make-module-name = $(subst /,-,$(1))

# Gets the path of the currently loaded module makefile.
get-this-module-path = $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

# Creates a variable `<module-name>-object-files' which contains the filepaths
# of the object files in the module, and appends the module source file to the
# `all-source-files' variable.
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
  $(1): $$(foreach mod,$$(MODULES_$(call make-program-name,$(1))),\
                   $$(call $$(call make-module-name,$$(mod))-object-files))
  $(1): LDFLAGS += $(LDFLAGS_$(call make-program-name,$(1)))
  $(1): LDLIBS += $(LDLIBS_$(call make-program-name,$(1)))
endef

# Executes doxygen if a `dox' file is found within the module.
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
                     $(MODULES_$(call make-program-name,$(prog)))))
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

.PHONY: tarball
tarball: all
	$(MAKE) clean clean-deps
	$(MAKE) docs
	tar -cvf $(TARBALL_FILENAME).tar ../$$(basename $$(pwd))
	gzip $(TARBALL_FILENAME).tar
