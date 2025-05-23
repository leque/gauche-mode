;;; -*- lexical-binding: t; -*-
(require 'assess)

(setq-default indent-tabs-mode nil)

(defmacro gauche-with-temp-buffer (contents &rest body)
  (declare (indent 1))
  `(save-window-excursion
     (with-temp-buffer
       (insert ,contents)
       (pop-to-buffer (current-buffer))
       (gauche-mode)
       (font-lock-fontify-buffer)
       ,@body)))

(defun gauche-paredit-with-input-to-temp-buffer (macro)
  (gauche-with-temp-buffer ""
    (enable-gauche-paredit-mode)
    (execute-kbd-macro macro)
    (buffer-string)))

(defun gauche-paredit-test-key-sequence (expected input)
  (expect (gauche-paredit-with-input-to-temp-buffer input)
          :to-equal expected))

(defun gauche-indentation= (indented)
  (assess-roundtrip-indentation= 'gauche-mode indented))

(defun gauche-explain-indentation= (indented)
  (let ((indented (if (functionp indented) (funcall indented) indented)))
    (assess-explain-roundtrip-indentation= 'gauche-mode indented)))

(put 'gauche-indentation= 'ert-explainer 'gauche-explain-indentation=)

(buttercup-define-matcher-for-unary-function :to-roundtrip-indentation-equal gauche-indentation=)
