parser: parser.hs
	ghc parser.hs

run: parser
	./parser < test.txt
