;;; gauche-mode.el --- A mode for editing Gauche Scheme codes -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2007-2015 OOHASHI Daichi

;; Author: OOHASHI Daichi <dico.leque.comicron@gmail.com>
;; Keywords: languages, lisp, gauche
;; URL: https://github.com/leque/gauche-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2") (compat "30.1.0.0"))

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

(require 'compat)
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
                 (const :tag "output all data" "#f"))
  :group 'gauche-mode)

(defcustom gauche-mode-pprint-procedure "pprint"
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

(defun gauche-mode-indent-define-record-type
    (state indent-point normal-indent)
  (let ((count (cl-case gauche-mode-define-record-type-syntax
                 ((srfi) 3)
                 ((r6rs) 1)
                 (t
                  (warn "Unknown syntax for define-record-type: %s"
                        gauche-mode-define-record-type-syntax)
                  1))))
    (lisp-indent-specform count state indent-point normal-indent)))

;; SRFI-35:
;;  (define-condition-type condition-type supertype predicate field-spec ...)
;; R6RS:
;;  (define-condition-type condition-type ‌‌supertype constructor predicate field-spec1 ...)
(defun gauche-mode-indent-define-condition-type
    (state indent-point normal-indent)
  (let ((count (or (condition-case nil
                       (progn
                         (forward-sexp 4)
                         (and (thing-at-point 'symbol)
                              4))
                     (scan-error nil))
                   3)))
    (lisp-indent-specform count state indent-point normal-indent)))

(defun gauche-mode-indent-while/until (state indent-point normal-indent)
  (let ((count (or (condition-case nil
                       (cl-loop for n from 2 upto 4
                                do (forward-sexp)
                                when (and (> n 2)
                                          (equal (thing-at-point 'symbol t) "=>"))
                                return n
                                finally return nil)
                     (scan-error nil))
                   1)))
    (lisp-indent-specform count state indent-point normal-indent)))

(defun gauche-mode-indent-define-cfn/define-cproc
    (state indent-point normal-indent)
  (let ((count (condition-case nil
                   (cl-block find-body
                     (let ((n 2))
                       (forward-sexp (+ n 1))
                       (while t
                         (let* ((sym (thing-at-point 'symbol t))
                                (i (cond ((equal sym "::")
                                          2)
                                         ((string-prefix-p ":" sym)
                                          1)
                                         (t
                                          (cl-return-from find-body n)))))
                           (forward-sexp i)
                           (cl-incf n i)))))
                 (scan-error 3))))
    (lisp-indent-specform count state indent-point normal-indent)))

(defvar gauche-mode-posix-char-set-names
  '(
    "alnum" "alpha" "blank" "cntrl" "digit" "graph"
    "lower" "print" "punct" "space" "upper" "xdigit"
    "ALNUM" "ALPHA" "BLANK" "CNTRL" "DIGIT" "GRAPH"
    "LOWER" "PRINT" "PUNCT" "SPACE" "UPPER" "XDIGIT"
    ))

(defvar gauche-mode-keywords
  ;; ((name indent highlight?) ...)
  `(
    ,@(let ((src (concat (file-name-directory (or load-file-name
                                                  (buffer-file-name)))
                         "gauche-keywords.el")))
        (with-temp-buffer
          (insert-file-contents src)
          (point-min)
          (read (current-buffer))))
    ;; ^a ... ^z
    ,@(cl-loop for c from ?a to ?z
               collect `(,(intern (format "^%c" c)) nil t))
    (^_ nil t)
    (make 1 nil)
    (make-parameter 1 nil)
    (define-cfn gauche-mode-indent-define-cfn/define-cproc t)
    (define-cproc gauche-mode-indent-define-cfn/define-cproc t)
    ;; R6RS
    (call-with-bytevector-output-port 0 nil)
    (call-with-string-output-port 0 nil)
    (datum->syntax 1 nil)
    (define-enumeration 1 nil)
    (identifier-syntax 0 nil)
    (library 1 nil)
    (with-syntax 1 nil)
    ))

(defcustom gauche-mode-extra-keywords '()
  "extra keywords for indentation/font-lock"
  :type '(repeat
          (list (symbol :tag "name of the keyword")
                (choice (const nil)
                        (integer
                         :tag "indent point for scheme-indent-function"))
                (boolean
                 :tag "highlight the keyword as font-lock-keyword or not")))
  )

(defun gauche-mode-keywords->font-lock-keyword (keywords)
  (let ((ks (cl-loop
             for (name indent highlight?) in keywords
             when indent do (put name 'scheme-indent-function indent)
             when highlight? collect (symbol-name name))))
    (and ks
         `((,(rx-to-string
              `(seq "("
                    (submatch-n 1 (or ,@ks))
                    symbol-end))
            1 font-lock-keyword-face)))))

(defvar gauche-mode-font-lock-keywords
  (append
   `(,@(gauche-mode-keywords->font-lock-keyword gauche-mode-keywords)
     (,(rx "("
           (submatch-n
            1
            (or "error" "errorf" "syntax-error" "syntax-errorf"))
           symbol-end)
      1 font-lock-warning-face)
     (,(rx "#\\"
           (or (1+ word)
               anything))
      0 font-lock-string-face)
     (,(rx symbol-start
           (or "<>" "<...>")
           symbol-end)
      0 font-lock-builtin-face t)
     (,(rx "#"
           (or (seq (any "Tt")
                    (opt (any "Rr") (any "Uu") (any "Ee")))
               (seq (any "Ff")
                    (opt (any "Aa") (any "Ll") (any "Ss") (any "Ee"))))
           symbol-end)
      0 font-lock-constant-face)
     (,(rx "#!" (1+ word))
      0 font-lock-comment-face)
     (,(rx buffer-start "#!" (0+ any))
      0 font-lock-preprocessor-face t)
     (,(rx (or "#?,"
               "#?="
               (seq "#" (1+ digit) (or "#" "="))))
      0 font-lock-preprocessor-face)
     )
   scheme-font-lock-keywords-1
   scheme-font-lock-keywords-2))

(defun gauche-syntax-propertize-sexp-comment (end)
  (static-if (cl-destructuring-bind (min-arity . _)
                 (func-arity #'scheme-syntax-propertize-sexp-comment) (= min-arity 1))
      (scheme-syntax-propertize-sexp-comment end)
    (scheme-syntax-propertize-sexp-comment (point) end)))

(defun gauche-syntax-propertize (beg end)
  (goto-char beg)
  (gauche-syntax-propertize-sexp-comment end)
  (funcall
   (syntax-propertize-rules
    ;; sexp comments
    ((rx (submatch "#") ";")
     (1 (prog1 "< cn"
          (gauche-syntax-propertize-sexp-comment end))))
    ;; tokens ending with "#"
    ((rx (or (seq "#\\"
                  (or (1+ word)
                      anything))
             (seq "#" (1+ digit) "#")))
     ;; Skip propertization here and use the syntax property specified elsewhere,
     ;; which resolves ambiguity by giving higher precedence to tokens ending with "#"
     ;; rather than tokens beginning with "#".
     ;; For example, in the sequence "#\#//", the propertizer will favor the token "#\#"
     ;; (character literal, ends with '#') over "#//" (regexp literal, begins with '#').
     )
    ;; regexps
    ((rx (submatch "#")
         "/"
         (0+ (or (seq "\\" anything)
                 (not (any "/\\"))
                 ))
         (or (seq "/" (submatch "i"))
             (submatch "/")))
     (1 "| 14")
     (2 "|")
     (3 "|"))
    ;; SRFI-14 Character-set
    ((rx (submatch "#")
         "["
         (0+ (or (seq "\\" anything)
                 (seq "[:" (0+ lower) ":]")
                 (not (any "[]\\"))))
         (submatch "]"))
     (1 "| 14")
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

(defun gauche-font-lock-syntactic-face-function (state)
  ;; (generic) string or (generic) comment
  (if (nth 3 state)
      ;; (generic) string
      (if (eq ?| (char-after (nth 8 state)))
          ;; escaped symbol
          nil
        font-lock-string-face)
    font-lock-comment-face))

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
  (setq scheme-program-name "gosh")
  (setq comment-start ";;")
  (setq font-lock-defaults
        `((,@(gauche-mode-keywords->font-lock-keyword
              gauche-mode-extra-keywords)
           ,@gauche-mode-font-lock-keywords)
          nil
          nil
          (("+-*/.<>=!?$%_&~^:" . "w"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)
          (parse-sexp-lookup-properties . t)
          (font-lock-syntactic-face-function
           . gauche-font-lock-syntactic-face-function)
          ))
  (setq-local syntax-propertize-function #'gauche-syntax-propertize)
  )

(defun gauche-mode-last-sexp ()
  (save-excursion
    (let* ((ep (point))
           (sp (progn (backward-sexp) (point))))
      (buffer-substring sp ep))))

(defun gauche-mode-export-current-symbol (renamep)
  (interactive "P")
  (let* ((symbol (thing-at-point 'symbol t))
         (exported-name (if renamep
                            (read-string "export-as: ")
                          symbol)))
    (unless symbol
      (error "no symbol at point"))
    (save-excursion
      (save-match-data
        (unless (re-search-backward (rx "(export" symbol-end)
                                    nil t)
          (error "No export clause found."))
        (let ((ep (condition-case nil
                      (save-excursion
                        (forward-sexp)
                        (point))
                    (scan-error
                     (error "Unclosed export clause.")))))
          (down-list)
          (cl-block nil
            (ignore-errors
              (while (< (point) ep)
                (forward-sexp)
                (skip-syntax-forward " ")
                (let ((thing
                       (cond ((looking-at (rx "(rename" symbol-end))
                              (ignore-errors
                                (save-excursion
                                  (down-list)
                                  (forward-sexp 2)
                                  (skip-syntax-forward " ")
                                  (thing-at-point 'symbol))))
                             (t
                              (thing-at-point 'symbol)))))
                  (when (equal exported-name thing)
                    (message "%s is already exported." symbol)
                    (cl-return nil)))))
            (goto-char (1- ep))
            (if renamep
                (insert " (rename " symbol " " exported-name ")")
              (insert " " symbol))
            (lisp-indent-line)
            (message "Exported %s." symbol)))))))

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

(defun gauche-mode-toggle-debug-funcall ()
  "toggle #?, (debug-funcall)"
  (interactive)
  (gauche-mode--toggle-symbol "#?,"))

(defun gauche-mode-toggle-datum-comment ()
  "toggle #; (datum comment)"
  (interactive)
  (gauche-mode--toggle-symbol "#;"))

(defun gauche-mode--paren-pairs (p)
  (cl-case p
    ((?\( ?\))
     '(?\[ . ?\]))
    ((?\[ ?\])
     '(?\( . ?\)))
    (t
     nil)))

(defun gauche-mode--toggle-paren-type (ch at-open)
  "toggle parentheses and brackets"
  (interactive)
  (let* ((p (gauche-mode--paren-pairs ch))
         (dir (if at-open 1 -1))
         (a (if at-open (car p) (cdr p)))
         (b (if at-open (cdr p) (car p))))
    (when p
      (save-excursion
        (delete-char 1)
        (insert a)
        (when at-open
          (backward-char dir))
        (condition-case nil
            (progn
              (forward-sexp dir)
              (delete-char (- dir))
              (insert b))
          (scan-error nil))))))

(defun gauche-mode-toggle-paren-type ()
  "toggle parentheses and brackets"
  (interactive)
  (let ((ch (char-after)))
    (cl-case ch
      ((?\( ?\[)
       (gauche-mode--toggle-paren-type ch t))
      ((?\) ?\])
       (gauche-mode--toggle-paren-type ch nil))
      (t nil))))

;;; info-look
(defun gauche-mode-setup-info-look ()
  "setup info-lookup based on `gauche-mode-info-language'"
  (interactive)
  (info-lookup-add-help
   :topic 'symbol
   :mode  'gauche-mode
   :regexp "[^()'\" \t\n]+"
   :ignore-case nil
   :parse-rule  nil
   :other-modes nil
   :doc-spec (cl-case gauche-mode-info-language
               ((en)
                '(("(gauche-refe.info)Function and Syntax Index" nil
                   "^[ \t]+-- [^:]+:[ \t]*" nil)
                  ("(gauche-refe.info)Module Index" nil
                   "^[ \t]+-- [^:]+:[ \t]*" nil)
                  ("(gauche-refe.info)Class Index" nil
                   "^[ \t]+-- [^:]+:[ \t]*" nil)
                  ("(gauche-refe.info)Variable Index" nil
                   "^[ \t]+-- [^:]+:[ \t]*" nil)
                  ))
               ((ja)
                '(("(gauche-refj.info)Index - 手続きと構文索引" nil
                   "^[ \t]+-+ [^:]+:[ \t]*" nil)
                  ("(gauche-refj.info)Index - モジュール索引" nil
                   "^[ \t]+-+ [^:]+:[ \t]*" nil)
                  ("(gauche-refj.info)Index - クラス索引" nil
                   "^[ \t]+-+ [^:]+:[ \t]*" nil)
                  ("(gauche-refj.info)Index - 変数索引" nil
                   "^[ \t]+-+ [^:]+:[ \t]*" nil)
                  ))
               (t
                (error "invalid gauche-mode-info-language: %s"
                       gauche-mode-info-language)))))

(gauche-mode-setup-info-look)

(add-to-list 'file-coding-system-alist
             '("gauche-refj\\.info.*" . utf-8))

(defun gauche-mode-info-candidates (&optional _pat)
  (mapcar #'car (info-lookup->completions 'symbol 'gauche-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(sld\\|sci\\|scm\\)\\'"
               . gauche-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist
             '("gosh" . gauche-mode))

(provide 'gauche-mode)
;;; gauche-mode.el ends here
