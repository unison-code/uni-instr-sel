parser: parser.hs
	ghc parser.hs

run: parser
	./parser < test.txt
#	./parser < patterns-dump.sexpr

clean:
	-rm *.hi
	-rm *.o

veryclean: clean
	-rm parser

.PHONY: clean veryclean
