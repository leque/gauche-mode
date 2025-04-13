# gauche-mode.el ![Build Status](https://github.com/leque/gauche-mode/actions/workflows/test.yml/badge.svg?branch=master)

An Emacs major mode for [Gauche Scheme Interpreter](http://practical-scheme.net/gauche/).

## Requirements

* Emacs 28.2 or later

## Installation

`M-x package-install-file`

## Features

* syntax highlighting for regexps, char-sets, etc.
* info-look for the reference manual
* indentation for Gauche-specific syntaxes

## Configuration

```
(setq gauche-mode-info-language 'en) ; or 'ja
```

## Keybindings
### gauche-mode-toggle-debug-print `C-c C-d`

```
(list |(fact 10))
;; -->
(list #?=(fact 10))

(list #|?=(fact 10))
;; -->
(list (fact 10))
```

### gauche-mode-toggle-datum-comment `C-c ;`

```
(list |(foo 42) (bar))
;; -->
(list #;(foo 42) (bar))

(list #|;(foo 42) (bar))
;; -->
(list (foo 42) (bar))
```

### gauche-mode-export-current-symbol `C-c M-x`

```
(define-module foo.bar
  (export))

(define |baz 42)
;; -->
(define-module foo.bar
  (export baz))

(define |baz 42)
```

With a prefix argument, you can export symbols with renaming.

```
(define-module foo.bar
  (export))

(define |baz 42)
;; C-u C-c M-x
;; export-as: quux
;; -->
(define-module foo.bar
  (export (rename baz quux)))

(define |baz 42)
```

### gauche-mode-macroexpand `C-c M-m`

```
(llist* 1 2 3)
|
;; -| (cons 1 (lcons* 2 3))
```

### gauche-mode-macroexpand-1 `C-c C-m`

```
(llist* 1 2 3)
|
;; -| (lcons* 1 2 3)
```

## gauche-paredit.el

Gauche-aware paredit-mode.

### How to use

`M-x enable-gauche-paredit-mode` or put below in your configuration file.

```
(add-hook 'gauche-mode-hook #'enable-gauche-paredit-mode)
```
