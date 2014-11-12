export CP_PATTERN_MATCHER_SCRIPT := /home/ghb/projects/zapbar/repo-int/src/inst-sel/pattern-matcher/minizinc/pattern-matcher

HLIBPATH := ../hlib

GHC      := ghc
GHCFLAGS += -Wall -i$(HLIBPATH)



.PHONY: param-test
param-test:
	$(GHC) $(GHCFLAGS) param-test.hs

.PHONY: run-param-test
run-param-test: param-test
	./param-test

.PHONY: match-test
match-test:
	$(GHC) $(GHCFLAGS) match-test.hs

.PHONY: run-match-test
run-match-test: match-test
	./match-test

.PHONY: test-frontend
test-frontend:
	$(GHC) $(GHCFLAGS) test-llvm.hs
	./test-llvm test.ll
	dot -Tpdf test.dot -o test.pdf

parser:
	$(GHC) $(GHCFLAGS) parser.hs

run: parser fact.ll
	./parser fact.ll
#	./parser < test.txt

test.pdf: test.dot
	dot -Tpdf test.dot -o test.pdf

clean:
	find . -name "*.hi" -type f -exec rm {} \;
	find . -name "*.o" -type f -exec rm {} \;

veryclean: clean
	-rm parser

.PHONY: parser clean veryclean
