parser: parser.hs
	ghc parser.hs

run: parser
	./parser < test.txt

clean:
	-rm *.hi
	-rm *.o

veryclean: clean
	-rm parser

.PHONY: clean veryclean
