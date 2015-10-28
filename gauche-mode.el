;;; -*- coding: utf-8 -*-
;;; gauche-mode.el --- A mode for editing Gauche Scheme codes in Emacs

;; Copyright (c) 2007-2015 OOHASHI Daichi

;; Author: OOHASHI Daichi <dico.leque.comicron@gmail.com>
;; Keywords: scheme, gauche
;; URL: https://github.com/leque/gauche-mode
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(eval-when-compile
  (require 'cl))

(require 'scheme)
(require 'cmuscheme)
(require 'info-look)

(defgroup gauche-mode nil
  "A mode for editing Gauche Scheme codes in Emacs"
  :prefix "gauche-mode-"
  :group 'applications)

(defcustom gauche-mode-info-language 'en
  "language of the reference manual to be shown"
  :type '(choice
          (const "English" en)
          (const "Japanese" ja))
  :group 'gauche-mode)

(defcustom gauche-mode-profiler-max-rows "#f"
  "max number of rows of profiler output"
  :type '(choice integer
                 (const "#f" :tag "output all data"))
  :group 'gauche-mode)

(defcustom gauche-mode-pprint-procedure "write"
  "pretty print procedure for output of macroexpand etc."
  :type 'string
  :group 'gauche-mode)

(defcustom gauche-mode-define-record-type-syntax 'srfi
  "syntax for indenting define-record-type"
  :type '(choice
          (const :tag "SRFI-99 / ERR5RS syntax" srfi)
          (const :tag "R6RS syntax" r6rs)
          )
  :group 'gauche-mode)

(make-variable-buffer-local 'gauche-mode-define-record-type-syntax)

(defun gauche-mode-switch-define-record-type ()
  (put 'define-record-type
       'scheme-indent-function
       (case gauche-mode-define-record-type-syntax
         ((srfi) 3)
         ((r6rs) 1)
         (t (error "Unknown syntax for define-record-type: %s"
                   gauche-mode-define-record-type-syntax)))))

(defun gauche-mode-use-srfi-define-record-type ()
  "indent `define-record-type' according to SRFI-99 syntax."
  (interactive)
  (setq gauche-mode-define-record-type-syntax 'srfi)
  (gauche-mode-switch-define-record-type))

(defun gauche-mode-use-r6rs-define-record-type ()
  "indent `define-record-type' according to R6RS syntax."
  (interactive)
  (setq gauche-mode-define-record-type-syntax 'r6rs)
  (gauche-mode-switch-define-record-type))

(defvar gauche-keywords
  ;; ((name indent highlight?) ...)
  `(
    (^ 1 t)
    (^_ 0 t)
    ;; ^a ... ^z
    ,@(loop for c from ?a to ?z
            collect `(,(intern (format "^%c" c)) 0 t))
    (and-let* 1 t)
    (begin0 0 t)
    (call-with-builder 1 nil)
    (call-with-cgi-script 1 nil)
    (call-with-client-socket 1 nil)
    (call-with-current-continuation 0 nil)
    (call-with-ftp-connection 1 nil)
    (call-with-input-conversion 1 nil)
    (call-with-input-file 1 nil)
    (call-with-input-process 1 nil)
    (call-with-input-string 1 nil)
    (call-with-iterator 1 nil)
    (call-with-iterators 1 nil)
    (call-with-output-conversion 1 nil)
    (call-with-output-file 1 nil)
    (call-with-output-process 1 nil)
    (call-with-output-string 0 nil)
    (call-with-process-io 1 nil)
    (call-with-string-io 1 nil)
    (call-with-values 1 nil)
    (call/cc 0 nil)
    (case-lambda nil t)
    (cond-expand nil t)
    (cond-list nil t)
    (cut nil t)
    (cute nil t)
    (define-condition-type 2 t)
    (define-constant nil t)
    (define-in-module nil t)
    (define-reader-ctor nil t)
    (define-record-type nil t)
    (define-values nil t)
    (delay nil t)
    (dolist 1 t)
    (dotimes 1 t)
    (dynamic-wind 0 nil)
    (eager nil t)
    (fluid-let 1 t)
    (guard 1 t)
    (hash-table-for-each 1 nil)
    (if-let1 2 t)
    (if-match 2 t)
    (lazy nil t)
    (let*-values 1 t)
    (let-args 2 t)
    (let-keywords 2 t)
    (let-keywords* 2 t)
    (let-optionals* 2 t)
    (let-syntax 2 t)
    (let-values 1 t)
    (let/cc 1 t)
    (let1 2 t)
    (letrec-syntax 2 t)
    (make 1 nil)
    (make-parameter 1 nil)
    (match 1 nil)
    (parameterize 1 t)
    (parse-options 1 t)
    (pre-pose-order 1 nil)
    (rec 1 t)
    (receive 2 t)
    (reset 0 t)
    (rlet1 2 t)
    (rxmatch-case 1 t)
    (rxmatch-cond 0 t)
    (rxmatch-if 4 t)
    (rxmatch-let 2 t)
    (shift 1 t)
    (syntax-rules 1 t)
    (unless 1 t)
    (until 1 t)
    (unwind-protect 1 t)
    (when 1 t)
    (while 1 t)
    (with-builder 1 t)
    (with-error-handler 1 nil)
    (with-error-to-port 1 nil)
    (with-exception-handler 1 nil)
    (with-input-conversion 1 nil)
    (with-input-from-file 1 nil)
    (with-input-from-port 1 nil)
    (with-input-from-process 1 nil)
    (with-input-from-string 1 nil)
    (with-iterator 1 t)
    (with-locking-mutex 1 nil)
    (with-lock-file 1 nil)
    (with-module 1 t)
    (with-output-conversion 1 nil)
    (with-output-to-file 1 nil)
    (with-output-to-port 1 nil)
    (with-output-to-process 1 nil)
    (with-port-locking 1 nil)
    (with-ports 3 nil)
    (with-signal-handlers 1 t)
    (with-string-io 1 nil)
    (with-time-counter 1 t)
    ;; R6RS
    (call-with-bytevector-output-port 0 nil)
    (call-with-port 1 nil)
    (call-with-string-output-port 0 nil)
    (datum->syntax 1 nil)
    (define-enumeration 1 nil)
    (identifier-syntax 0 nil)
    (letrec* 1 nil)
    (library 1 nil)
    (with-syntax 1 nil)
    ))

(defvar gauche-mode-font-lock-keywords
  (append
   `((,(format "(%s\\>"
               (regexp-opt
                (loop for (name indent highlight?) in gauche-keywords
                      when indent do (put name 'scheme-indent-function indent)
                      when highlight? collect (symbol-name name))
                t))
      1 font-lock-keyword-face)
     (,(format "(%s\\>"
               (regexp-opt
                '("error" "errorf" "syntax-error" "syntax-errorf") t))
      1 font-lock-warning-face)
     (,(format "\\<%s\\>"
               (regexp-opt
                '("<>" "<...>") t))
      0 font-lock-builtin-face t)
     ("#!\\w+"
      0 font-lock-comment-face)
     ("\\`#!.*"
      0 font-lock-preprocessor-face t)
     ("\\(#\\?=\\|#[0-9]+#\\|#[0-9]+=\\)"
      0 font-lock-preprocessor-face)
     )
   scheme-font-lock-keywords-1
   scheme-font-lock-keywords-2))

(defvar gauche-mode-syntax-table
  (let ((syntax (copy-syntax-table scheme-mode-syntax-table)))
    (modify-syntax-entry ?\| "  23b" syntax)
    (modify-syntax-entry ?\# "' 14bp" syntax)
    syntax))

(defun gauche-syntax-propertize (beg end)
  (goto-char beg)
  (scheme-syntax-propertize-sexp-comment (point) end)
  (funcall
   (syntax-propertize-rules
    ("\\(#\\);" (1 (prog1 "< cn"
                     (scheme-syntax-propertize-sexp-comment (point) end))))
    ("#\\(/\\)\\(\\(\\\\\\\\\\)+\\|\\\\[^\\]\\|[^/\\]\\)*\\(/\\)"
     (1 "\"")
     (4 "\""))
    ;; R6RS inline hex escape
    ("\\\\[xX][0-9a-zA-Z]+\\(;\\)"
     (1 "_"))
    ;; R6RS bytevectors
    ("#\\(vu8\\)("
     (1 "'"))
    ;; R7RS bytevectors + SRFI-4 Homogeneous numeric vector datatypes
    ("#\\([su]\\(8\\|16\\|32\\|64\\)\\|f\\(16\\|32\\|64\\)\\)("
     (1 "'"))
    )
   (point) end))

(defvar gauche-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?d)] #'gauche-mode-toggle-debug-print)
    (define-key map [(control ?c) (meta ?x)] #'gauche-mode-export-current-symbol)
    (define-key map [(control ?c) (meta ?d)] #'gauche-mode-disassemble)
    (define-key map [(control ?c) (meta ?m)] #'gauche-mode-macroexpand)
    (define-key map [(control ?c) (control ?m)] #'gauche-mode-macroexpand-1)
    (define-key map [(control ?c) (control ?p)] #'gauche-mode-profile-last-sexp)
    (define-key map [(control ?c) ?\;] #'gauche-mode-toggle-datum-comment)
    map))

(define-derived-mode gauche-mode scheme-mode
  "Gauche" "Major mode for Gauche."
  (use-local-map gauche-mode-map)
  (set-syntax-table gauche-mode-syntax-table)
  (setq scheme-program-name "gosh")
  (gauche-mode-switch-define-record-type)
  (setq comment-start ";;")
  (setq font-lock-defaults
        `(,gauche-mode-font-lock-keywords
          nil
          t
          (("+-*/.<>=!?$%_&~^:" . "w"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)
          (parse-sexp-lookup-properties . t)
          ))
  (setq-local syntax-propertize-function #'gauche-syntax-propertize)
  )

(defun gauche-mode-last-sexp ()
  (save-excursion
    (let* ((ep (point))
           (sp (progn (backward-sexp) (point))))
      (buffer-substring sp ep))))

(defun gauche-mode-export-current-symbol ()
  (interactive)
  (let ((word (thing-at-point 'sexp)))
    (save-excursion
      (unless (re-search-backward "^\\s *(export\\Sw+" nil t)
        (error "No export clause found."))
      (let ((bp (match-beginning 0)))
        (unless (re-search-forward "\\s)" nil t)
          (error "Unclosed export clause."))
        (let ((ep (match-beginning 0)))
          (goto-char bp)
          (if (re-search-forward
               (concat "\\s " (regexp-quote word) "[ \t\n\f\v\)]") (1+ ep) t)
              (message "%s is already exported." word)
            (goto-char ep)
            (insert " " word)
            (lisp-indent-line)
            (message "Exported %s." word)))))))

(defun gauche-mode-macroexpand (arg &optional n)
  "Expands the last macro and print it on *scheme* buffer.
With universal-argument, do not unwrap syntax."
  (interactive "P")
  (let ((exp (gauche-mode-last-sexp))
        (f (if arg "values" "unwrap-syntax")))
    (comint-send-string
     (scheme-proc)
     (format "(begin (newline) (%s (%s (%%macroexpand%s %s))))\n"
             gauche-mode-pprint-procedure
             f (or n "") exp))))

(defun gauche-mode-macroexpand-1 (arg)
  "Similar to gauche-mode-macroexpand,
but use macroexpand-1 instead."
  (interactive "P")
  (gauche-mode-macroexpand arg "-1"))

(defun gauche-mode-profile-last-sexp (key)
  (interactive (list (completing-read "Sort result by: "
                                      '("time" "count" "time-per-call")
                                      nil t "time" nil)))
  (let ((exp (gauche-mode-last-sexp)))
    (comint-send-string
     (scheme-proc)
     (format "(unwind-protect
                  (begin (newline) (profiler-reset) (profiler-start) %s)
                  (begin (profiler-stop)
                         (profiler-show :sort-by '%s :max-rows %s)))\n"
             exp key gauche-mode-profiler-max-rows))))

(defun gauche-mode-disassemble (exp)
  (interactive (list (read-string "Disassemble: "
                                  (or (thing-at-point 'sexp)
                                      (gauche-mode-last-sexp)))))
  (comint-send-string
   (scheme-proc)
   (format "(begin (newline) (disasm %s))\n" exp)))

(defun gauche-mode-toggle-debug-print ()
  (interactive)
  (let ((p (point))
        (c (char-after))
        (d (char-before)))
    (cond
     ((null c) t)                       ; do nothing
     ((and d
           (char-equal d ?=)
           (char-equal (char-before (- p 1)) ??)
           (char-equal (char-before (- p 2)) ?#))
      (delete-region (- p 3) p))
     ((and (char-equal c ?#)
           (char-equal (char-after (+ p 1)) ??)
           (char-equal (char-after (+ p 2)) ?=))
      (delete-region p (+ p 3)))
     ((and (char-equal c ??)
           (char-equal (char-after (- p 1)) ?#)
           (char-equal (char-after (+ p 1)) ?=))
      (delete-region (- p 1) (+ p 2)))
     ((and (char-equal c ?=)
           (char-equal (char-after (- p 1)) ??)
           (char-equal (char-after (- p 2)) ?#))
      (delete-region (- p 2) (+ p 1)))
     (t
      (insert "#?=")))))

(defun gauche-mode-toggle-datum-comment ()
  (interactive)
  (let ((p (point))
        (b (char-before))
        (c (char-after)))
    (cond
     ((null c) t)                       ; do nothing
     ((and b
           (char-equal b ?\;)
           (char-equal (char-before (- p 1)) ?#))
      (delete-region (- p 2) p))
     ((and (char-equal b ?#)
           (char-equal c ?\;))
      (delete-region (- p 1) (+ p 1)))
     ((and (char-equal c ?#)
           (char-equal (char-after (+ p 1)) ?\;))
      (delete-region p (+ p 2)))
     (t
      (insert "#;")))))

;;; info-look
(defun gauche-mode-setup-info-look ()
  "setup info-lookup based on `gauche-mode-info-language'"
  (interactive)
  (eval-after-load "info-look"
    (case gauche-mode-info-language
      ((en)
       '(info-lookup-add-help
         :topic 'symbol
         :mode  'gauche-mode
         :regexp "[^()'\" \t\n]+"
         :ignore-case nil
         :doc-spec '(("(gauche-refe.info)Function and Syntax Index" nil
                      "^[ \t]+-- [^:]+:[ \t]*" nil)
                     ("(gauche-refe.info)Module Index" nil
                      "^[ \t]+-- [^:]+:[ \t]*" nil)
                     ("(gauche-refe.info)Class Index" nil
                      "^[ \t]+-- [^:]+:[ \t]*" nil)
                     ("(gauche-refe.info)Variable Index" nil
                      "^[ \t]+-- [^:]+:[ \t]*" nil))
         :parse-rule  nil
         :other-modes nil))
      ((ja)
       '(info-lookup-add-help
         :topic 'symbol
         :mode  'gauche-mode
         :regexp "[^()'\" \t\n]+"
         :ignore-case nil
         :doc-spec '(("(gauche-refj.info)Index - 手続きと構文索引" nil
                      "^[ \t]+-+ [^:]+:[ \t]*" nil)
                     ("(gauche-refj.info)Index - モジュール索引" nil
                      "^[ \t]+-+ [^:]+:[ \t]*" nil)
                     ("(gauche-refj.info)Index - クラス索引" nil
                      "^[ \t]+-+ [^:]+:[ \t]*" nil)
                     ("(gauche-refj.info)Index - 変数索引" nil
                      "^[ \t]+-+ [^:]+:[ \t]*" nil))
         :parse-rule  nil
         :other-modes nil))
      (t
       (error "invalid gauche-mode-info-language: %s"
              gauche-mode-info-language)))))

(gauche-mode-setup-info-look)

(defun gauche-mode-info-candidates (_pat)
  (mapcar #'car (info-lookup->completions 'symbol 'gauche-mode)))

(provide 'gauche-mode)
