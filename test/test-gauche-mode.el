;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'gauche-mode)

(load (concat (file-name-directory (or load-file-name
                                       (buffer-file-name)))
              "utils.el"))

(describe "gauche-mode-indent-define-record-type"
  (it "indents R6RS define-record-type (gauche-mode-define-record-type-syntax = r6rs)"
    (let ((gauche-mode-define-record-type-syntax 'r6rs))
      (expect "\
(define-record-type pare
  (fields kar kdr))
"
       :to-roundtrip-indentation-equal)))
  (it "indents R6RS define-record-type (gauche-mode-define-record-type-syntax = srfi)"
    (let ((gauche-mode-define-record-type-syntax 'srfi))
      (expect "\
(define-record-type pare
    (fields kar kdr))
"
       :to-roundtrip-indentation-equal)))
  (it "indents SRFI define-record-type (gauche-mode-define-record-type-syntax = r6rs)"
    (let ((gauche-mode-define-record-type-syntax 'r6rs))
      (expect "\
(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))
"
       :to-roundtrip-indentation-equal)))
  (it "indents SRFI define-record-type (gauche-mode-define-record-type-syntax = srfi)"
    (let ((gauche-mode-define-record-type-syntax 'srfi))
      (expect "\
(define-record-type :pare
    (kons x y)
    pare?
  (x kar set-kar!)
  (y kdr))
"
       :to-roundtrip-indentation-equal)))
  )

(describe "gauche-mode-indent-define-condition-type"
  (it "indents SRFI-35 define-record-type (newline before predicate)"
    (expect "\
(define-condition-type &c &condition
                       c?
  (x c-x))
"
     :to-roundtrip-indentation-equal))
  (it "indents SRFI-35 define-record-type (newline before supertype)"
    (expect "\
(define-condition-type &c
    &condition
    c?
  (x c-x))
"
     :to-roundtrip-indentation-equal))
  (it "indents R6RS define-record-type (newline before constructor)"
    (expect "\
(define-condition-type &c &condition
                       make-c c?
  (x c-x))
"
     :to-roundtrip-indentation-equal))
  (it "indents R6RS define-record-type (newline before supertype)"
    (expect "\
(define-condition-type &c
    &condition
    make-c
    c?
  (x c-x))
"
     :to-roundtrip-indentation-equal))
  )

(describe "gauche-mode-indent-while/until"
  (it "indents while"
    (expect "\
(while expr
  body)
"
     :to-roundtrip-indentation-equal))
  (it "indents while without body"
    (expect "\
(while expr
  )
"
     :to-roundtrip-indentation-equal))
  (it "indents while (newline before condition)"
    (expect "\
(while
    expr
  )
"
     :to-roundtrip-indentation-equal))
  (it "indents while with `=> var`"
    (expect "\
(while expr
    => var
  body)
"
     :to-roundtrip-indentation-equal))
  (it "indents while with guard"
    (expect "\
(while expr
    guard
    => var
  body)
"
     :to-roundtrip-indentation-equal))
  (it "indents while with guard (newline before `=>`)"
    (expect "\
(while expr guard
       => var
  body)
"
     :to-roundtrip-indentation-equal))
  (it "indents while (expr = `=>`)"
    (expect "\
(while =>
  body)
"
     :to-roundtrip-indentation-equal))
  (it "indents while (`=>` in body)"
    (expect "\
(while expr
  e1
  e2
  =>
  body)
"
     :to-roundtrip-indentation-equal))
  )

(describe "gauche-mode-indent-define-cfn/define-cproc"
  (it "indents define-cproc"
    (expect "\
(define-cproc foo () ::<int> :constant
  1)
"
     :to-roundtrip-indentation-equal))
  (it "indents define-cproc (newline before ret-type)"
    (expect "\
(define-cproc foo ()
              ::<int>
              :constant
  1)
"
     :to-roundtrip-indentation-equal))
  (it "indents define-cfn"
    (expect "\
(define-cfn foo () ::<int> :constant
  1)
"
     :to-roundtrip-indentation-equal))
  (it "indents define-cfn (newline before ret-type)"
    (expect "\
(define-cfn foo ()
            ::<int>
            :constant
  1)
"
     :to-roundtrip-indentation-equal))
  )

(describe "gauche-mode-indent-lambda-formals"
  (it "indents lambda with keyword parameters only"
    (expect "\
(lambda (:key (x 1)
              (y 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents lambda with required and keyword parameters"
    (expect "\
(lambda (x :key (y 1)
                (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents lambda with required and keyword parameters (newline before `:key`)"
    (expect "\
(lambda (x
         :key (y 1)
              (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents lambda with required and keyword parameters (newline before each keyword parameter)"
    (expect "\
(lambda (x
         :key
           (y 1)
           (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents lambda with required and keyword parameters (comments between parameters)"
    (expect "\
(lambda (x
         :key ; comment
           (y 1)
           (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents lambda with keyword and optional parameters"
    (expect "\
(lambda (:key (x 1)
         :optional (y 2))
  x)
"
     :to-roundtrip-indentation-equal))
  ;; In the example below, `:rest` is currently aligned with `:key`,
  ;; but it might be better to align `:rest` with `:optional` instead.
  (it "indents lambda with optional, keyword, and rest parameters (optional and keyword parameters on the same line)"
    (expect "\
(lambda (:optional x :key y
                     :rest z)
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents lambda with keyword and optional parameters (multiple optional parameters)"
    (expect "\
(lambda (:key (x 1)
         :optional (y 2)
                   (z 3))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents lambda with required, keyword, and optional parameters"
    (expect "\
(lambda (x :key (y 1)
           :optional (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents higher-order define with keyword and optional parameters (mixed)"
    (expect "\
(define ((foo x) :key (y 1)
                 :optional (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents higher-order define with keyword and optional parameters"
    (expect "\
(define ((foo x :key w) :key (y 1)
                        :optional (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents higher-order define with keyword and optional parameters (with comments)"
    (expect "\
(define (; comment
         (foo x :key w) :key (y 1)
                        :optional (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents higher-order define with keyword and optional parameters (multi-line)"
    (expect "\
(define ((foo x
              :key w)
         :key (y 1)
         :optional (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents define-method with optional parameters"
    (expect "\
(define-method dict-get ((m <empty-dict>)
                         :optional
                           default)
  default)
"
     :to-roundtrip-indentation-equal))
  (it "indents define-method with optional parameters and a qualifier"
    (expect "\
(define-method dict-get :locked ((m <empty-dict>)
                                 :optional
                                   default)
  default)
"
     :to-roundtrip-indentation-equal))
  (it "indents define-method with optional parameters (+ block comments)"
    (expect "\
(define-method #||# dict-get #||# :locked #||# ((m <empty-dict>)
                                                :optional
                                                  default)
  default)
"
     :to-roundtrip-indentation-equal))
  )

(describe "gauche-mode-toggle-paren-type"
  (it "toggles () to [] (outer)"
    (expect (gauche-with-temp-buffer "|(a (b c) d)"
                (:point-at "|")
              (goto-char (point-min))
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "[a (b c) d]"))
  (it "toggles [] to () (outer)"
    (expect (gauche-with-temp-buffer "|[a (b c) d]"
                (:point-at "|")
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a (b c) d)"))
  (it "toggles () to [] (inner)"
    (expect (gauche-with-temp-buffer "(a |(b c) d)"
                (:point-at "|")
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a [b c] d)"))
  (it "toggles () to [] (inner. point at close paren)"
    (expect (gauche-with-temp-buffer "(a (b c|) d)"
                (:point-at "|")
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a [b c] d)"))
  (it "toggles [] to () (outer. point at close bracket)"
    (expect (gauche-with-temp-buffer "[a (b c) d|]"
                (:point-at "|")
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a (b c) d)"))
  (it "toggles () to [] (outer. point at close paren)"
    (expect (gauche-with-temp-buffer "(a (b c) d|)"
                (:point-at "|")
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "[a (b c) d]"))
  (it "toggles () to [] (outer. unclosed)"
    (expect (gauche-with-temp-buffer "|(a (b c) d"
                (:point-at "|")
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "[a (b c) d"))
  (it "toggles () to [] (inner. outer unclosed)"
    (expect (gauche-with-temp-buffer "(a |(b c) d"
                (:point-at "|")
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a [b c] d"))
  (it "toggles () to [] (inner, outer unclosed #2)"
    (expect (gauche-with-temp-buffer "(a |(b c d)"
                (:point-at "|")
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a [b c d]"))
  (it "toggles () to [] (outer. unclosed. point at close paren)"
    (expect (gauche-with-temp-buffer "a (b c) d|)"
                (:point-at "|")
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "a (b c) d]"))
  )

(describe "font-lock"
  (it "highlights a sexp comment"
    (expect "#;(a b c) 1"
            :to-be-font-locked-as
            `((,(rx "#;") font-lock-comment-face)
              (,(rx "(a b c") font-lock-comment-face)
              (,(rx ")") font-lock-comment-delimiter-face)
              (,(rx "1") nil)
              )))
  (it "highlights a regexp literal"
    (expect "#/a/ #/a/i #/\\w/"
            :to-be-font-locked-as
            `((,(rx "#/a/") font-lock-string-face)
              (,(rx "#/a/i") font-lock-string-face)
              (,(rx "#/\\w/") font-lock-string-face)
              )))
  (it "highlights a char-set literal"
    (expect "#[[:alpha:]] #[[:^alpha:]] #[[:ALPHA:]] #[[:^ALPHA:]] #[a-z] #[] #[\\w]"
            :to-be-font-locked-as
            `((,(rx "#[[:alpha:]]") font-lock-string-face)
              (,(rx "#[[:^alpha:]]") font-lock-string-face)
              (,(rx "#[[:ALPHA:]]") font-lock-string-face)
              (,(rx "#[[:^ALPHA:]]") font-lock-string-face)
              (,(rx "#[a-z]") font-lock-string-face)
              (,(rx "#[]") font-lock-string-face)
              (,(rx "#[\\w]") font-lock-string-face)
              )))
  (it "highlights a boolean literal"
    (expect "#t #f #True #False #TRUE #FALSE"
            :to-be-font-locked-as
            `(
              (,(rx "#true") font-lock-constant-face)
              (,(rx "#false") font-lock-constant-face)
              (,(rx "#True") font-lock-constant-face)
              (,(rx "#False") font-lock-constant-face)
              (,(rx "#TRUE") font-lock-constant-face)
              (,(rx "#FALSE") font-lock-constant-face)
              )))
  (it "does not highlight a non-boolean lookalike"
    (expect "#truer #falsetto"
            :to-be-font-locked-as
            `(
              (,(rx "#truer") nil)
              (,(rx "#falsetto") nil)
              )))
  (it "highlights a hex-escape in piped symbols"
    (expect "|\\x3bb;|"
            :to-be-font-locked-as
            `((,(rx ";") nil))))
  (it "interprets an ambuiguous # as a part of the preceding datum"
    (expect "#\\#//"
            :to-be-font-locked-as
            `((,(rx "#\\#") font-lock-string-face)
              (,(rx "//") nil)
              )))
  )

(describe "gauche-syntax-propertize"
  (it "treats :|ab cd| as a single datum"
    (expect (gauche-with-temp-buffer "_:|ab cd|"
                (:point-at "_")
              (forward-sexp)
              (eobp))
            :to-be-truthy))
  (it "treats #:|ab cd| as a single datum"
    (expect (gauche-with-temp-buffer "_#:|ab cd|"
                (:point-at "_")
              (forward-sexp)
              (eobp))
            :to-be-truthy))
  (it "treats #[[:alpha:]] as a single datum"
    (expect (gauche-with-temp-buffer "_#[[:alpha:]]"
                (:point-at "_")
              (forward-sexp)
              (eobp))
            :to-be-truthy))
  (it "treats #[[^:alpha:]] as a single datum"
    (expect (gauche-with-temp-buffer "_#[[^:alpha:]]"
                (:point-at "_")
              (forward-sexp)
              (eobp))
            :to-be-truthy))
  (it "treats #[[:ALPHA:]] as a single datum"
    (expect (gauche-with-temp-buffer "_#[[:ALPHA:]]"
                (:point-at "_")
              (forward-sexp)
              (eobp))
            :to-be-truthy))
  )

(describe "gauche-mode-export-current-symbol"
  (it "exports the current symbol"
    (expect (gauche-with-temp-buffer "\
(define-module gauche-mode.test
  (export)

(define |one 1)
"
                (:point-at "|")
              (execute-kbd-macro (kbd "M-x gauche-mode-export-current-symbol RET"))
              (buffer-string))
            :to-equal "\
(define-module gauche-mode.test
  (export one)

(define one 1)
"
            ))
  (it "exports the current symbol (line comment inside the export list)"
    (expect (gauche-with-temp-buffer "\
(define-module gauche-mode.test
  (export one ; comment
          )

(define one 1)
(define |two 2)
"
                (:point-at "|")
              (execute-kbd-macro (kbd "M-x gauche-mode-export-current-symbol RET"))
              (buffer-string))
            :to-equal "\
(define-module gauche-mode.test
  (export one ; comment
          two)

(define one 1)
(define two 2)
"
            ))
  (it "exports the current symbol (block comment inside the export list)"
    (expect (gauche-with-temp-buffer "\
(define-module gauche-mode.test
  (export one #|comment|#)

(define one 1)
(define _two 2)
"
                (:point-at "_")
              (execute-kbd-macro (kbd "M-x gauche-mode-export-current-symbol RET"))
              (buffer-string))
            :to-equal "\
(define-module gauche-mode.test
  (export one #|comment|# two)

(define one 1)
(define two 2)
"
            ))
  (it "exports the current symbol (sexp comment inside the export list)"
    (expect (gauche-with-temp-buffer "\
(define-module gauche-mode.test
  (export one #;two)

(define one 1)
(define _two 2)
"
                (:point-at "_")
              (execute-kbd-macro (kbd "M-x gauche-mode-export-current-symbol RET"))
              (buffer-string))
            :to-equal "\
(define-module gauche-mode.test
  (export one #;two two)

(define one 1)
(define two 2)
"
            ))
  (it "exports the current symbol with renaming"
    (expect (gauche-with-temp-buffer "\
(define-module gauche-mode.test
  (export)

(define |one 1)
"
                (:point-at "|")
              (execute-kbd-macro (kbd "C-u M-x gauche-mode-export-current-symbol RET uno RET"))
              (buffer-string))
            :to-equal "\
(define-module gauche-mode.test
  (export (rename one uno))

(define one 1)
"
            ))
  (it "does not modify the export list if the current symbol is already exported"
    (expect (gauche-with-temp-buffer "\
(define-module gauche-mode.test
  (export one)

(define |one 1)
"
                (:point-at "|")
              (execute-kbd-macro (kbd "M-x gauche-mode-export-current-symbol RET"))
              (buffer-string))
            :to-equal "\
(define-module gauche-mode.test
  (export one)

(define one 1)
"
            ))
  (it "does not modify the export list if the current symbol is already exported (with renaming)"
    (expect (gauche-with-temp-buffer "\
(define-module gauche-mode.test
  (export uno)

(define |one 1)
(define uno one)
"
                (:point-at "|")
              (execute-kbd-macro (kbd "C-u M-x gauche-mode-export-current-symbol RET uno RET"))
              (buffer-string))
            :to-equal "\
(define-module gauche-mode.test
  (export uno)

(define one 1)
(define uno one)
"
            ))
  )
