EMACS ?= emacs
CASK ?= cask
LOADPATH = -L .
ELPA_DIR = \
    $(shell emacs=$(EMACS) $(CASK) package-directory)
GOSH ?= gosh
GAUCHE_SRC =
GEN_KEYWORDS = tools/gen-keywords-list.scm
ELC = gauche-mode.elc gauche-paredit.elc
ERROR_ON_WARN ?= t

.PHONY: test compile clean

test: $(ELPA_DIR)
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test/test.el \
		-f ert-run-tests-batch-and-exit

compile: $(ELPA_DIR) $(ELC)

.el.elc:
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		--eval '(setq byte-compile-error-on-warn $(ERROR_ON_WARN))' \
		-f batch-byte-compile $<

clean:
	rm -f $(ELC)

$(ELPA_DIR): Cask
	$(CASK) install
	touch $@

gauche-keywords.el: $(GAUCHE_SRC)/doc/gauche-refe.texi $(GEN_KEYWORDS)
	$(GOSH) $(GEN_KEYWORDS) -o $@ $<
