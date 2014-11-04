HLIBPATH := ../hlib

GHC      := ghc
GHCFLAGS += -Wall -i$(HLIBPATH)



.PHONY: param-test
param-test:
	$(GHC) $(GHCFLAGS) param-test.hs

.PHONY: match-test
match-test:
	$(GHC) $(GHCFLAGS) match-test.hs

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
