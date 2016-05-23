;;; gauche-mode.el --- A mode for editing Gauche Scheme codes -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2007-2015 OOHASHI Daichi

;; Author: OOHASHI Daichi <dico.leque.comicron@gmail.com>
;; Keywords: languages, lisp, gauche
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

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'scheme)
(require 'cmuscheme)
(require 'info-look)
(require 'rx)

(defgroup gauche-mode nil
  "A mode for editing Gauche Scheme codes in Emacs"
  :prefix "gauche-mode-"
  :group 'applications)

(defcustom gauche-mode-info-language 'en
  "language of the reference manual to be shown"
  :type '(choice
          (const :tag "English" en)
          (const :tag "Japanese" ja))
  :group 'gauche-mode
  :set #'(lambda (symbol value)
           (set-default symbol value)
           (when (fboundp 'gauche-mode-setup-info-look)
             (gauche-mode-setup-info-look))))

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
       (cl-case gauche-mode-define-record-type-syntax
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
    ;; ^a ... ^z
    ,@(cl-loop for c from ?a to ?z
               collect `(,(intern (format "^%c" c)) 0 t))
    ($ nil t)
    (%macroexpand nil t)
    (%macroexpand-1 nil t)
    (^ 1 t)
    (^_ 0 t)
    (add-load-path nil t)
    (address-family nil t)
    (address-info nil t)
    (and nil t)
    (and-let* 1 t)
    (and-let1 2 t)
    (any?-ec nil t)
    (append-ec nil t)
    (apropos nil t)
    (autoload 1 t)
    (begin 0 t)
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
    (call-with-port 1 nil)
    (call-with-process-io 1 nil)
    (call-with-string-io 1 nil)
    (call-with-values 1 nil)
    (call/cc 0 nil)
    (case 1 t)
    (case-lambda nil t)
    (cgen-with-cpp-condition 1 t)
    (cond nil t)
    (cond-expand nil t)
    (cond-list nil t)
    (condition nil t)
    (current-module nil t)
    (cut nil t)
    (cute nil t)
    (debug-print nil t)
    (dec! nil t)
    (define nil t)
    (define-cise-expr nil t)
    (define-cise-macro nil t)
    (define-cise-stmt nil t)
    (define-cise-toplevel nil t)
    (define-class nil t)
    (define-condition-type 2 t)
    (define-constant nil t)
    (define-dict-interface nil t)
    (define-generic nil t)
    (define-in-module nil t)
    (define-library nil t)
    (define-macro nil t)
    (define-method nil t)
    (define-module nil t)
    (define-reader-ctor nil t)
    (define-record-type nil t)
    (define-syntax nil t)
    (define-values nil t)
    (delay 0 t)
    (delay-force 0 t)
    (do 2 t)
    (do-ec nil t)
    (do-generator 1 t)
    (dolist 1 t)
    (dotimes 1 t)
    (dynamic-wind 0 nil)
    (eager 0 nil)
    (ecase 1 t)
    (every?-ec nil t)
    (export nil t)
    (export-all nil t)
    (extend nil t)
    (first-ec nil t)
    (fluid-let 1 t)
    (fold-ec nil t)
    (fold3-ec nil t)
    (get-keyword* nil t)
    (get-optional nil t)
    (glet* 1 t)
    (glet1 2 t)
    (guard 1 t)
    (if nil t)
    (if-let1 2 t)
    (if-not=? nil t)
    (if3 nil t)
    (if<=? nil t)
    (if<? nil t)
    (if=? nil t)
    (if>=? nil t)
    (if>? nil t)
    (import nil t)
    (inc! nil t)
    (include nil t)
    (include-ci nil t)
    (ip-protocol nil t)
    (lambda 1 t)
    (last-ec nil t)
    (lazy 0 t)
    (lcons nil t)
    (lcons* nil t)
    (let nil t)
    (let* 1 t)
    (let*-values 1 t)
    (let-args 2 t)
    (let-keywords 2 t)
    (let-keywords* 2 t)
    (let-optionals* 2 t)
    (let-string-start+end 4 t)
    (let-syntax 1 t)
    (let-values 1 t)
    (let/cc 1 t)
    (let1 2 t)
    (letrec 1 t)
    (letrec* 1 t)
    (letrec-syntax 1 t)
    (list-ec nil t)
    (llist* nil t)
    (make 1 nil)
    (make-option-parser 0 t)
    (make-parameter 1 nil)
    (match 1 t)
    (match-define defun t)
    (match-lambda 0 t)
    (match-lambda* 0 t)
    (match-let 1 t)
    (match-let* 1 t)
    (match-let1 2 t)
    (match-letrec 1 t)
    (max-ec nil t)
    (message-type nil t)
    (min-ec nil t)
    (or nil t)
    (parameterize 1 t)
    (parse-options 1 t)
    (pop! nil t)
    (product-ec nil t)
    (push! nil t)
    (quasiquote nil t)
    (quote nil t)
    (rec 1 t)
    (receive 2 t)
    (require nil t)
    (require-extension nil t)
    (reset 0 t)
    (rlet1 2 t)
    (rxmatch-case 1 t)
    (rxmatch-cond 0 t)
    (rxmatch-if 4 t)
    (rxmatch-let 2 t)
    (select-module nil t)
    (set! nil t)
    (set!-values nil t)
    (shift 1 t)
    (shutdown-method nil t)
    (socket-domain nil t)
    (ssax:make-elem-parser nil t)
    (ssax:make-parser nil t)
    (ssax:make-pi-parser nil t)
    (stream-cons nil t)
    (stream-delay 0 t)
    (string-append-ec nil t)
    (string-ec nil t)
    (sum-ec nil t)
    (syntax-error nil t)
    (syntax-errorf nil t)
    (syntax-rules 1 t)
    (test* nil t)
    (time 0 t)
    (unless 1 t)
    (unquote nil t)
    (unquote-splicing nil t)
    (until 1 t)
    (unwind-protect 1 t)
    (update! nil t)
    (use nil t)
    (values->list nil t)
    (values-ref nil t)
    (vector-ec nil t)
    (vector-of-length-ec nil t)
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
    (with-lock-file 1 nil)
    (with-locking-mutex 1 nil)
    (with-module 1 t)
    (with-output-conversion 1 nil)
    (with-output-to-file 1 nil)
    (with-output-to-port 1 nil)
    (with-output-to-process 1 nil)
    (with-output-to-string 0 nil)
    (with-port-locking 1 nil)
    (with-ports 3 nil)
    (with-random-data-seed 1 nil)
    (with-signal-handlers 1 t)
    (with-string-io 1 nil)
    (with-time-counter 1 t)
    (xml-token-head nil t)
    (xml-token-kind nil t)
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
   `((,(rx-to-string
        `(seq "("
              (submatch-n
               1
               (or ,@(cl-loop
                      for (name indent highlight?) in gauche-keywords
                      when indent do (put name 'scheme-indent-function indent)
                      when highlight? collect (symbol-name name))))
              symbol-end))
      1 font-lock-keyword-face)
     (,(rx "("
           (submatch-n
            1
            (or "error" "errorf" "syntax-error" "syntax-errorf"))
           symbol-end)
      1 font-lock-warning-face)
     (,(rx symbol-start
           (or "<>" "<...>")
           symbol-end)
      0 font-lock-builtin-face t)
     (,(rx "#!" (1+ word))
      0 font-lock-comment-face)
     (,(rx buffer-start "#!" (0+ any))
      0 font-lock-preprocessor-face t)
     (,(rx (or "#?="
               (seq "#" (1+ digit) (or "#" "="))))
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
    ;; sexp comments
    ((rx (submatch "#") ";")
     (1 (prog1 "< cn"
          (scheme-syntax-propertize-sexp-comment (point) end))))
    ;; regexps
    ((rx (submatch "#")
         "/"
         (0+ (or (seq "\\" any)
                 (not (any "/\\"))
                 ))
         (or (seq "/" (submatch "i"))
             (submatch "/")))
     (1 "| cn")
     (2 "|")
     (3 "|"))
    ;; SRFI-14 Character-set
    ((rx (submatch "#")
         "["
         (0+ (or (seq "\\" any)
                 (seq "[:" (0+ lower) ":]")
                 (not (any "[]\\"))))
         (submatch "]"))
     (1 "| cn")
     (2 "|"))
    ;; R6RS inline hex escape
    ((rx "\\" (any "Xx") (1+ hex-digit) (submatch ";"))
     (1 "_"))
    ;; R6RS bytevectors
    ((rx "#" (submatch "vu8") "(")
     (1 "'"))
    ;; R7RS bytevectors + SRFI-4 Homogeneous numeric vector datatypes
    ((rx "#"
         (submatch
          (or (seq (any "f") (or "16" "32" "64"))
              (seq (any "su") (or "8" "16" "32" "64"))))
         "(")
     (1 "'"))
    )
   (point) end))

(defvar gauche-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'gauche-mode-toggle-debug-print)
    (define-key map (kbd "C-c M-x") #'gauche-mode-export-current-symbol)
    (define-key map (kbd "C-c M-d") #'gauche-mode-disassemble)
    (define-key map (kbd "C-c M-m") #'gauche-mode-macroexpand)
    (define-key map (kbd "C-c C-m") #'gauche-mode-macroexpand-1)
    (define-key map (kbd "C-c C-p") #'gauche-mode-profile-last-sexp)
    (define-key map (kbd "C-c   ;") #'gauche-mode-toggle-datum-comment)
    map))

;;;###autoload
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
      (save-match-data
        (unless (re-search-backward (rx "(export" symbol-end)
                                    nil t)
          (error "No export clause found."))
        (let ((bp (match-beginning 0)))
          (unless (re-search-forward (rx ")")
                                     nil t)
            (error "Unclosed export clause."))
          (let ((ep (match-beginning 0)))
            (goto-char bp)
            (if (re-search-forward (rx-to-string
                                    `(seq symbol-start ,word symbol-end))
                                   (1+ ep) t)
                (message "%s is already exported." word)
              (goto-char ep)
              (insert " " word)
              (lisp-indent-line)
              (message "Exported %s." word))))))))

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

(defun gauche-mode--toggle-symbol (sym)
  (let* ((p (point))
         (len (length sym))
         (i (cl-loop for i from 0 downto (- len)
                     when (cl-loop for j from 0 below len
                                   always (eql (aref sym j)
                                               (char-after (+ p i j))))
                     return i)))
    (if i
        (delete-region (+ p i) (+ p i len))
      (insert sym))))

(defun gauche-mode-toggle-debug-print ()
  "toggle #?= (debug-print)"
  (interactive)
  (gauche-mode--toggle-symbol "#?="))

(defun gauche-mode-toggle-datum-comment ()
  "toggle #; (datum comment)"
  (interactive)
  (gauche-mode--toggle-symbol "#;"))

;;; info-look
(defun gauche-mode-setup-info-look ()
  "setup info-lookup based on `gauche-mode-info-language'"
  (interactive)
  (eval-after-load "info-look"
    (cl-case gauche-mode-info-language
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

(defun gauche-mode-info-candidates (&optional _pat)
  (mapcar #'car (info-lookup->completions 'symbol 'gauche-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons "\\.\\(sld\\|sci\\|scm\\)\\'"
                   'gauche-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist
             (cons "gosh" 'gauche-mode))

(provide 'gauche-mode)
;;; gauche-mode.el ends here
