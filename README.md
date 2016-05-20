# gauche-mode.el

An Emacs major mode for [Gauche Scheme Interpreter](http://practical-scheme.net/gauche/).

## Requirements

* Emacs 24.4 or later

## Installation

`M-x package-install-file`

## Features

* syntax highlighting for regexps
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
