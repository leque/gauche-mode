(use parser.peg)

(define %dots
  ($do (($string "@dots{}"))
       ($return '...)))

(define %dot
  ($->string
   ($char #\.)))

(define %lambda-list-keyword
  ($try
   ($or ($seq ($string ":key")
              ($return ':key))
        ($seq ($string ":optional")
              ($return ':optional)))))

(define %symbol
  ($->string ($many1 ($none-of #[(){}\[\]\s]))))

(define %code
  ($do (($string "@code{"))
       (sym %symbol)
       (($char #\}))
       ($return sym)))

(define %var
  ($do (($string "@var{"))
       (sym %symbol)
       (($char #\}))
       ($return sym)))

(define %setter
  ($do (($string "{(setter "))
       (spaces)
       (sym %symbol)
       (spaces)
       (($string ")}"))
       ($return `(setter ,sym))))

(define %list
  ($lazy
   ($do (($char #\())
        (exprs %exprs)
        (spaces)
        (($char #\)))
        ($return exprs))))

(define %opt
  ($lazy
   ($do (($char #\[))
        (exprs %exprs)
        (spaces)
        (($char #\]))
        ($return (cons 'opt exprs)))))

(define %exprs
  ($lazy
   ($many ($seq spaces %expr))))

(define %expr
  ($lazy
   ($or %list %opt %dots %dot %lambda-list-keyword %code %var %setter %symbol)))

(define %spec
  ($do (es %exprs)
       (spaces)
       (eof)
       ($return es)))

(define (parse-spec str)
  (guard (exc
          ((<parse-error> exc)
           (errorf "parse-error: ~A: ~S"
                   (ref exc'message)
                   str)))
    (peg-parse-string %spec str)))

(use srfi-1)
(use srfi-2)
(use srfi-13)
(use gauche.generator)
(use gauche.parseopt)
(use gauche.sequence)
(use util.match)

(define (read-info port)
  (filter values
          (generator-map
           (lambda (line)
             (rxmatch-case line
               (#/^@def(?:mac|spec)x? (.*)$/ (#f spec)
                (cons ':syntax (parse-spec spec)))
               (#/^@defunx? (.*)$/ (#f spec)
                (cons ':proc (parse-spec spec)))
               (#/^@deffnx? {(?:Generic function|Method)} (.*)$/ (#f spec)
                (cons ':proc (parse-spec spec)))
               (else #f)))
           (port->line-generator port))))

(define (calc-indent spec)
  (define (calc-syntax-indent n args)
    (match args
      (() 'nil)
      ;; with-foo x ... thunk
      ;; for with-signal-handlers
      (("thunk")
       n)
      ((x y '...)
       (if (and (string? x)
                (string? y)
                (string-prefix? (string-trim-right x #[0-9])
                                y))
           ;; foo x ... e0 e1 ...
           (if (zero? n)
               'nil
               n)
           (calc-syntax-indent (+ n 1) (list y '...))))
      ((x '...)
       ;; foo x ... body ...
       (if (zero? n)
           'nil
           n))
      ((_ . rest)
       (calc-syntax-indent (+ n 1) rest))))
  (define (f name indent highlight?)
    `(,(string->symbol name) ,indent ,highlight?))
  (match spec
    ((':syntax "^c" . _)
     ;; not handled here
     #f)
    ((':proc (and name "call-with-values") . _)
     (f name 1 'nil))
    ((':syntax (and name "define-condition-type") . _)
     (f name
        'gauche-mode-indent-define-condition-type
        't))
    ((':syntax (and name "define-record-type") . _)
     (f name
        'gauche-mode-indent-define-record-type
        't))
    ((_typ (and name
                (? string?)
                (or "match-define"
                    (? #/^define\b/)))
           . _)
     ;; NB: define-reader-ctor is a procedure. But highlight it anyway.
     (f name 'defun 't))
    ((':syntax (and name (or "syntax-error" "syntax-errorf")) . _)
     (f name 'nil 't))
    ((':syntax (and name (or "let" "syntax-rules" "match-let")) . _)
     (f name 'scheme-let-indent 't))
    ((':syntax (and name (or "let-syntax" "letrec-syntax")) . _)
     (f name 1 't))
    ((':syntax (and name "parse-options") . _)
     (f name 1 't))
    ((':syntax (and name "quasirename") . _)
     (f name 1 't))
    ((':syntax (and name (or "while" "until")) . _)
     (f name 'gauche-mode-indent-while/until 't))
    ((':syntax (and name
                    (? string?)
                    (or (? #/-if$/)
                        (? #/^if(?![a-z0-9])/)))
               . rest)
     (f name
        (let ((l (length (delete ':optional rest))))
          (if (= l 3) 'nil (- l 2)))
        't))
    ((':proc (and name
                  (? string?)
                  (? #/^(with-|call\/|call-with-)/)
                  )
             . args)
     (f name
        (or (find-index (cut member <> '("proc" "thunk")) args) 'nil)
        'nil))
    ((':syntax name . args)
     (f name (calc-syntax-indent 0 args) 't))
    (_ #f)))

(define (usage out name status)
  (format out "\
usage: ~A -o output.lisp /path/to/gauche-refe.texi
"
          name)
  (exit status))

(define (main args)
  (define (error-usage)
    (usage (current-error-port) (car args) 1))
  (let-args (cdr args)
      ((outfile "o=s")
       . rest)
    (unless outfile
      (error-usage))
    (call-with-output-file outfile
      (lambda (out)
        (match rest
          ((infile)
           (format out ";;; Generated from ~A. DO NOT EDIT~%"
                   (sys-basename infile))
           (format out "(~%")
           (for-each
            (cut format out " ~S~%" <>)
            (delete-duplicates
             (filter-map calc-indent
                         (sort (call-with-input-file infile read-info)
                               default-comparator
                               (lambda (x)
                                 (and x
                                      (list (cadr x) (- (length x)))))))
             (lambda (x y)
               (equal? (car x) (car y)))))
           (format out " )~%")
           0)
          (_
           (error-usage)))))))
