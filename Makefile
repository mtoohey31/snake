main: *.hs **/*.hs Term/Size.hs
	ghc -dynamic main

Term/Size.hs: Term/Size.hsc
	hsc2hs Term/Size.hsc

.PHONY: clean

clean:
	rm *.{hi,o} Term/Size.hs
