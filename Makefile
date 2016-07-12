EMACS ?= emacs
CASK ?= cask
LOADPATH = -L .
ELPA_DIR = \
    $(shell emacs=$(EMACS) $(CASK) package-directory)

.PHONY: test

test: $(ELPA_DIR)
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test/test.el \
		-f ert-run-tests-batch-and-exit

$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
