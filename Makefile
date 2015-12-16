# Copyright (c) 2013-2015, Gabriel Hjort Blindell <ghb@kth.se>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#  * Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
#
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



#==========
# SETTINGS
#==========

UNIDISTPATH := dist
SOLVERSPATH := solvers
TOOLSPATH := tools


#=======
# RULES
#=======

.PHONY: build
build: uni-is uni-targen

.PHONY: docs
docs: uni-is-doc uni-targen-doc

.PHONY: uni-is
uni-is:
	$(RM) *.cabal
	ln -s uni-is.cabal.package uni-is.cabal
	cabal install
	rm uni-is.cabal
	cd $(SOLVERSPATH) && make
	cd $(TOOLSPATH) && make

.PHONY: uni-is-doc
uni-is-doc:
	$(RM) *.cabal
	ln -s uni-is.cabal.package uni-is.cabal
	cabal configure
	cabal haddock --executables
	rm uni-is.cabal

.PHONY: uni-targen
uni-targen:
	$(RM) *.cabal
	ln -s uni-targen.cabal.package uni-targen.cabal
	cabal install
	rm uni-targen.cabal

.PHONY: uni-targen-doc
uni-targen-doc:
	$(RM) *.cabal
	ln -s uni-targen.cabal.package uni-targen.cabal
	cabal configure
	cabal haddock --executables
	rm uni-targen.cabal

.PHONY: clean
clean:
	$(RM) -rf $(UNIDISTPATH)
	cd $(SOLVERSPATH) && make clean
	cd $(TOOLSPATH) && make clean

.PHONY: distclean
distclean:
	$(RM) -rf $(UNIDISTPATH)
	cd $(SOLVERSPATH) && make distclean
	cd $(TOOLSPATH) && make distclean
