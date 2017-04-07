EMACS ?= emacs
CASK ?= cask
LOADPATH = -L .
ELPA_DIR = \
    $(shell emacs=$(EMACS) $(CASK) package-directory)
GOSH ?= gosh
GAUCHE_SRC =
GEN_KEYWORDS = tools/gen-keywords-list.scm

.PHONY: test

test: $(ELPA_DIR)
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test/test.el \
		-f ert-run-tests-batch-and-exit

$(ELPA_DIR): Cask
	$(CASK) install
	touch $@

gauche-keywords.el: $(GAUCHE_SRC)/doc/gauche-refe.texi $(GEN_KEYWORDS)
	$(GOSH) $(GEN_KEYWORDS) -o $@ $<
