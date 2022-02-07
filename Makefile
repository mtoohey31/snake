build: term.size
	ghc -dynamic main

term.size:
	hsc2hs Term/Size.hsc

clean:
	rm *.{hi,o}
