EMACS ?= emacs
CASK ?= cask
LOADPATH = -L .
ELPA_DIR = \
    .cask/$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')/elpa

.PHONY: test

test: $(ELPA_DIR)
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test/test.el \
		-f ert-run-tests-batch-and-exit

$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
