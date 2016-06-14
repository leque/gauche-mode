EMACS = emacs -L .
RUN_TEST = $(EMACS) -Q -batch

.PHONY: test

test:
	$(RUN_TEST) -l test/test.el -f ert-run-tests-batch-and-exit
