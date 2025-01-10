.PHONY: all-tests

all-tests: *.elt
	echo OK

%.elt: %.el ../*.el ../src/*.el
	emacs -batch -l ert -L ../src -l $< -f ert-run-tests-batch-and-exit
	touch $@
