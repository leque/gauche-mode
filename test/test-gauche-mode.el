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
  (it "indents lambda with ordinary and keyword parameters"
    (expect "\
(lambda (x :key (y 1)
                (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents lambda with ordinary and keyword parameters (newline before `:key`)"
    (expect "\
(lambda (x
         :key (y 1)
              (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents lambda with ordinary and keyword parameters (newline before each keyword parameters)"
    (expect "\
(lambda (x
         :key
           (y 1)
           (z 2))
  x)
"
     :to-roundtrip-indentation-equal))
  (it "indents lambda with ordinary and keyword parameters (with comments in the middle of parameters)"
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
  (it "indents lambda with optional, keyword, and rest parameters (optional and keyword parameters are placed in the same line)"
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
  (it "indents lambda with ordinary, keyword, and optional parameters"
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
  (it "indents define-method with optional parameters and qualifier"
    (expect "\
(define-method dict-get :locked ((m <empty-dict>)
                                 :optional
                                   default)
  default)
"
     :to-roundtrip-indentation-equal))
  (it "indents define-method with optional parameters (+block comments)"
    (expect "\
(define-method #||# dict-get #||# :locked #||# ((m <empty-dict>)
                                                :optional
                                                  default)
  default)
"
     :to-roundtrip-indentation-equal))
  )

(describe "gauche-mode-toggle-paren-type"
  (it "toggles [] to ()"
    (expect (gauche-with-temp-buffer "(a (b c) d)"
              (goto-char (point-min))
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "[a (b c) d]"))
  (it "toggles () to []"
    (expect (gauche-with-temp-buffer "[a (b c) d]"
              (goto-char (point-min))
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a (b c) d)"))
  (it "toggles [] to () (nested case)"
    (expect (gauche-with-temp-buffer "(a (b c) d)"
              (goto-char 4)
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a [b c] d)"))
  (it "toggles [] to () (nested case. point at close bracket)"
    (expect (gauche-with-temp-buffer "(a (b c) d)"
              (goto-char 8)
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a [b c] d)"))
  (it "toggles () to [] (point at close paren)"
    (expect (gauche-with-temp-buffer "[a (b c) d]"
              (goto-char (point-max))
              (backward-char 1)
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a (b c) d)"))
  (it "toggles [] to () (point at close bracket)"
    (expect (gauche-with-temp-buffer "(a (b c) d)"
              (goto-char (point-max))
              (backward-char 1)
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "[a (b c) d]"))
  (it "toggles [] to () (unclosed)"
    (expect (gauche-with-temp-buffer "(a (b c) d"
              (goto-char (point-min))
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "[a (b c) d"))
  (it "toggles () to [] (unclosed)"
    (expect (gauche-with-temp-buffer "(a (b c) d"
              (goto-char 4)
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a [b c] d"))
  (it "toggles [] to () (nested, unclosed)"
    (expect (gauche-with-temp-buffer "(a (b c d)"
              (goto-char 4)
              (gauche-mode-toggle-paren-type)
              (buffer-string))
            :to-equal "(a [b c d]"))
  )
