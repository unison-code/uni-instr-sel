parser:
	ghc -Wall parser.hs

run: parser
	./parser < test.txt
	dot -Tpdf test.dot -o test.pdf
#	./parser < patterns-dump.sexpr

clean:
	find . -name "*.hi" -type f -exec rm {} \;
	find . -name "*.o" -type f -exec rm {} \;

veryclean: clean
	-rm parser

.PHONY: parser clean veryclean
