all-tests: $(patsubst %.el,%.elt,$(wildcard *.el))
	echo OK: $^

%.elt: %.el ../lisp/*.el erts/*.erts erts/**/*.erts
	emacs -batch -l ert --eval "(package-initialize)" -L .. -L ../lisp -l $< -f ert-run-tests-batch-and-exit
	touch $@

clean:
	rm -rf *.elt
