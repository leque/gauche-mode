;;; -*- lexical-binding: t; -*-
(require 'compat)
(require 'assess)

(setq-default indent-tabs-mode nil)

(cl-defmacro gauche-with-temp-buffer (contents (&key point-at) &rest body)
  (declare (indent 2))
  (cl-once-only (point-at)
    `(save-window-excursion
       (with-temp-buffer
         (insert ,contents)
         (pop-to-buffer (current-buffer))
         (gauche-mode)
         (font-lock-fontify-buffer)
         (goto-char (point-min))
         (if ,point-at
             (when (search-forward ,point-at nil t)
               (delete-region (match-beginning 0) (match-end 0)))
           (goto-char (point-max)))
         ,@body))))

(defun gauche-paredit-with-input-to-temp-buffer (macro)
  (gauche-with-temp-buffer "" ()
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

(defun gauche--parse-pat&faces (pat&faces)
  (cl-loop for (pat face) in pat&faces
           collect pat into pats
           collect face into faces
           finally return (list pats faces)))

(defun gauche-face-at= (text pat&faces)
  (cl-destructuring-bind (pats faces) (gauche--parse-pat&faces pat&faces)
    (assess-face-at= text 'gauche-mode pats faces)))

(defun gauche-explain-face-at= (text pat&faces)
  (cl-destructuring-bind (pats faces) (gauche--parse-pat&faces pat&faces)
    (assess-explain-face-at= text 'gauche-mode pats faces)))

(put 'gauche-face-at= 'ert-explainer 'gauche-explain-face-at=)

(buttercup-define-matcher-for-binary-function :to-be-font-locked-as gauche-face-at=)
