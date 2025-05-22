;;; -*- lexical-binding: t; -*-
(require 'gauche-paredit)
(require 'buttercup)

(load (concat (file-name-directory (or load-file-name
                                       (buffer-file-name)))
              "utils.el"))

(describe "gauche-paredit-slash"
  (it "automatically inserts close `/` for `#/`"
    (gauche-with-temp-buffer ""
      (enable-gauche-paredit-mode)
      (execute-kbd-macro (kbd "#/"))
      (expect (buffer-string)
              :to-equal "#//")))
  (it "deletes `#//` with DEL"
    (gauche-with-temp-buffer "#//"
      (enable-gauche-paredit-mode)
      (font-lock-fontify-buffer)
      (execute-kbd-macro (kbd "DEL"))
      (expect (buffer-string)
              :to-equal "")))
  (it "automatically inserts close `/` for `#/` (non-empty case)"
    (gauche-with-temp-buffer ""
      (enable-gauche-paredit-mode)
      (execute-kbd-macro (kbd "#/a"))
      (expect (buffer-string)
              :to-equal "#/a/")))
  (it "prevents deleting non-empty regexp with DEL"
    (gauche-with-temp-buffer "#/a/"
      (enable-gauche-paredit-mode)
      (font-lock-fontify-buffer)
      (execute-kbd-macro (kbd "DEL"))
      (expect (buffer-string)
              :to-equal "#/a/")))
  )

(describe "gauche-paredit-space-for-delimiter-p"
  (it "prevents inserting a space before `\"` in `#\"\"`"
    (gauche-paredit-test-key-sequence
     "#\"\""
     (kbd "# \"")))
  (it "prevents inserting a space before `\"` in `#*\"\"`"
    (gauche-paredit-test-key-sequence
     "#*\"\""
     (kbd "# * \"")))
  (it "prevents inserting a space before `\"` in `#**\"\"`"
    (gauche-paredit-test-key-sequence
     "#**\"\""
     (kbd "# * * \"")))
  (it "prevents inserting a space before `(` in `#n=()`"
    (dotimes (_ 100)
      (let ((n (random 1000)))
        (gauche-paredit-test-key-sequence
         (format "#%s=()" n)
         (kbd (format "# %s = (" n))))))
  (it "prevents inserting a space before `\"` in `#n=\"\"`"
    (dotimes (_ 100)
      (let ((n (random 1000)))
        (gauche-paredit-test-key-sequence
         (format "#%s=\"\"" n)
         (kbd (format "# %s = \"" n))))))
  (it "prevents inserting a space before `(` in `#?=()`"
    (gauche-paredit-test-key-sequence
     "#?=()"
     (kbd "# ? = (")))
  (it "prevents inserting a space before `\"` in `#?=\"\"`"
    (gauche-paredit-test-key-sequence
     "#?=\"\""
     (kbd "# ? = \"")))
  (it "prevents inserting a space before `x` in `#?=x`"
    (gauche-paredit-test-key-sequence
     "#?=x"
     (kbd "# ? = x")))
  (it "prevents inserting a space before `(` in `#??=()`"
    (gauche-paredit-test-key-sequence
     "#??=()"
     (kbd "# ? ? = (")))
  (it "prevents inserting a space before `\"` in `#??=\"\"`"
    (gauche-paredit-test-key-sequence
     "#??=\"\""
     (kbd "# ? ? = \"")))
  (it "prevents inserting a space before `x` in `#??=x`"
    (gauche-paredit-test-key-sequence
     "#??=x"
     (kbd "# ? ? = x")))
  (it "prevents inserting a space before `(` in `#vu8()`"
    (gauche-paredit-test-key-sequence
     "(f #vu8())"
     (kbd "(f SPC #vu8(")))
  (it "prevents inserting a space signed/unsigned vector prefix"
    (cl-loop for n in '(8 16 32 64)
             for p in '(s u)
             do (gauche-paredit-test-key-sequence
                 (format "(f #%s%s())" p n)
                 (kbd (format "(f SPC #%s%s(" p n)))))
  (it "inserts a space before `(` in `#s48 ()`"
    (gauche-paredit-test-key-sequence
     "(f #s48 ())"
     (kbd "(f SPC #s48(")))
  (it "prevents inserting a space after float vector prefix"
    (cl-loop for n in '(16 32 64)
             do (gauche-paredit-test-key-sequence
                 (format "(f #f%s())" n)
                 (kbd (format "(f SPC #f%s(" n)))))
  (it "inserts a space before `(` in `#f8 ()`"
    (gauche-paredit-test-key-sequence
     "(f #f8 ())"
     (kbd "(f SPC #f8(")))
  (it "prevents inserting a space before formals of `^` syntax"
    (gauche-paredit-test-key-sequence
     "(^(x) x)"
     (kbd "(^(x) SPC x")))
  (it "automatically inserts a space before `(`"
    (gauche-paredit-test-key-sequence
     "(f (g) h)"
     (kbd "(f(g) SPC h")))
  )
