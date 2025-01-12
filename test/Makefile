all-tests: $(patsubst %.el,%.elt,$(wildcard *.el))
	echo OK: $^

%.elt: %.el ../*.el ../src/*.el erts/*.erts erts/**/*.erts
	emacs -batch -l ert --eval "(package-initialize)" -L .. -L ../src -l $< -f ert-run-tests-batch-and-exit
	touch $@

clean:
	rm -rf *.elt
