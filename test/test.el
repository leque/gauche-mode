(require 'ert)
(require 'gauche-mode)

(defmacro gauche-with-temp-buffer (contents &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (gauche-mode)
     (font-lock-fontify-buffer)
     ,@body))

(defun gauche-test-string (str)
  (replace-regexp-in-string (rx bol (* space) "|")
                            ""
                            str))

(ert-deftest gauche-mode-indent-define-record-type-test/r6rs ()
  (gauche-with-temp-buffer
      (gauche-test-string
       "(define-record-type pare
       |(fields kar kdr))
       |")
    (setq gauche-mode-define-record-type-syntax 'r6rs)
    (goto-char (point-min))
    (indent-sexp)
    (should (equal (buffer-string)
                   (gauche-test-string
                    "(define-record-type pare
                    |  (fields kar kdr))
                    |")))
    (setq gauche-mode-define-record-type-syntax 'srfi)
    (goto-char (point-min))
    (indent-sexp)
    (should (equal (buffer-string)
                   (gauche-test-string
                    "(define-record-type pare
                    |    (fields kar kdr))
                    |")))))

(ert-deftest gauche-mode-indent-define-record-type-test/srfi ()
  (gauche-with-temp-buffer
      (gauche-test-string
       "(define-record-type :pare
       |(kons x y)
       |pare?
       |(x kar set-kar!)
       |(y kdr))
       |")
    (setq gauche-mode-define-record-type-syntax 'r6rs)
    (goto-char (point-min))
    (indent-sexp)
    (should (equal (buffer-string)
                   (gauche-test-string
                    "(define-record-type :pare
                    |  (kons x y)
                    |  pare?
                    |  (x kar set-kar!)
                    |  (y kdr))
                    |")))
    (setq gauche-mode-define-record-type-syntax 'srfi)
    (goto-char (point-min))
    (indent-sexp)
    (should (equal (buffer-string)
                   (gauche-test-string
                    "(define-record-type :pare
                    |    (kons x y)
                    |    pare?
                    |  (x kar set-kar!)
                    |  (y kdr))
                    |")))))
