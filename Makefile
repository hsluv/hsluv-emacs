build: hsluv.elc

hsluv.elc:
	emacs -batch -q -f batch-byte-compile hsluv.el

test : build
	emacs -batch -q  -l hsluv.elc -l test/hsluv-test.el -l test/launch-test.el 
