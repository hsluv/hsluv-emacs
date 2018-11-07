build: hsluv.elc

hsluv.elc:
	emacs -batch -q -f batch-byte-compile hsluv.el

test : clean build
	emacs -batch -q  -l hsluv.elc -l test/hsluv-test.el -l test/launch-test.el 2>&1 | tee test-results.log

clean:
	rm -f *.elc
	rm -f test-results.log
