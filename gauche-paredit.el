;;; gauche-paredit.el --- Paredit support for Gauche  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 OOHASHI Daichi

;; Author: OOHASHI Daichi <dico.leque.comicron@gmail.com>
;; URL: https://github.com/leque/gauche-mode
;; Package-Requires: ((gauche-mode "0.1.0") (paredit))
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

;;; (add-hook 'gauche-mode-hook #'enable-gauche-paredit-mode)

;;; Code:
(require 'rx)
(require 'nadvice)
(require 'paredit)
(require 'gauche-mode)

(defvar-keymap gauche-paredit-mode-map
  "/" #'gauche-paredit-slash
  "[" #'gauche-paredit-open-square
  "]" #'gauche-paredit-close-square
  )

;;;###autoload
(define-minor-mode gauche-paredit-mode
  "Minor-mode for Gauche-aware paredit"
  :keymap gauche-paredit-mode-map
  (setq-local paredit-space-for-delimiter-predicates
              (list #'gauche-paredit-space-for-delimiter-p))
  (paredit-mode (if gauche-paredit-mode +1 -1)))

(defvar gauche-paredit-paren-prefix-pat
  (rx
   (or
    (seq "#" (+ digit) "=")                      ; R7RS datum labels
    (seq "#" (any "su") (or "8" "16" "32" "64")) ; R7RS bytevectors + SRFI-4
    (seq "#" (any "f") (or "16" "32" "64"))      ; SRFI-4 + Gauche's extension
    "#vu8"                                       ; R6RS bytevectors
    "(^"                                         ; (^(x y) ...)
    "#?,"                                        ; debug-funcall
    "#?="                                        ; debug-print
    )))

(defun gauche-paredit-space-for-delimiter-p (endp delimiter)
  (or endp
      (if (= (char-syntax delimiter) ?\()
          (not (looking-back gauche-paredit-paren-prefix-pat nil))
        t)))

(defun gauche-paredit-in-char-set-p ()
  "True if the parse state within a char-set literal"
  (and (paredit-in-string-p)
       (gauche-paredit--in-char-set-p0)))

(defun gauche-paredit--in-char-set-p0 ()
  (save-excursion
    (goto-char (car (paredit-string-start+end-points)))
    (looking-at-p (rx "#["))))

(defun gauche-paredit-in-regexp-p ()
  "True if the parse state within a regexp literal"
  (and (paredit-in-string-p)
       (gauche-paredit--in-regexp-p0)))

(defun gauche-paredit--in-regexp-p0 ()
  (save-excursion
    (goto-char (car (paredit-string-start+end-points)))
    (looking-at-p (rx "#/"))))

(defun gauche-paredit-slash (&optional n)
  "After `#`, insert a pair of slashes.
At the opening/closing slash of a regexp, move past the slash.
In the middle of a regexp, insert a backslash-escaped slash.
Otherwise, insert a literal slash."
  (interactive "P")
  (cond ((gauche-paredit-in-regexp-p)
         (let* ((pair (paredit-string-start+end-points))
                (start (car pair))
                (end (cdr pair))
                (pos (point)))
           (cond
            ;; #|/../, #/..|/, or #/..|/i
            ((or (= pos (1+ start))
                 (= pos (if (= ?\/ (char-after end))
                            end
                          (1- end))))
             (forward-char))
            ;; #/../|i
            ((= pos end)
             (insert ?\/))
            ;; #/..|../
            (t
             (insert ?\\ ?\/)))))
        ((paredit-in-comment-p)
         (insert ?\/))
        ((not (paredit-in-char-p))
         (if (= (char-before) ?\#)
             (paredit-insert-pair n ?\/ ?\/ 'paredit-forward-for-quote)
           (insert ?\/)))))

(defun gauche-paredit-open-square (&optional n)
  "Insert a balanced pair of square brackets.
If not inside a char-set, simply delegate to `paredit-open-square'.

At the opening square bracket of a char-set, move past the bracket.
In the middle of a char-set, read one more character.
If the character is ':', insert a POSIX character set,
otherwise insert a backslash-escaped square bracket."
  (interactive "P")
  (if (not (gauche-paredit-in-char-set-p))
      (paredit-open-square n)
    (let ((p (paredit-string-start+end-points)))
      (if (= (point) (1+ (car p)))
          ;; #|[..]
          (forward-char)
        (let ((ch (read-char-exclusive)))
          (if (eql ch ?\:)
              (let ((name (completing-read
                           "char-set name: "
                           gauche-mode-posix-char-set-names)))
                (insert "[:" name ":]"))
            (insert "\\[")))))))

(defun gauche-paredit-close-square ()
  "Move past one closing delimiter and reindent.
If not inside a char-set, simply delegate to `paredit-close-square'.

At the end of a char-set, move past the closing square bracket.
In the middle of a char-set, insert a backslash-escaped square bracket."
  (interactive)
  (if (not (gauche-paredit-in-char-set-p))
      (paredit-close-square)
    (let* ((start+end (paredit-string-start+end-points))
           (beg (car start+end))
           (end (cdr start+end))
           (p (point)))
      (cond
       ;; #|[..]
       ((= p (1+ beg))
        (goto-char (1+ end)))
       ;; #[..|]
       ((= p end)
        (forward-char))
       ;; #[..|..]
       (t
        (insert "\\]"))))))

(defun gauche-paredit-around-forward-delete (f &optional argument &rest _args)
  "around advice for `paredit-forward-delete'.

Handle forward deletion immediately before regexps and char-sets.
If an expression is empty, delete the expression,
otherwise move forward into the expression."
  (cond ((or (not gauche-paredit-mode)
             (consp argument)
             (integerp argument)
             (eobp))
         (funcall f argument))
        ;; |#/../, |#[..]
        ((and (eql ?\# (char-after))
              (not (paredit-in-string-p))
              (save-excursion
                (forward-char)
                (and (paredit-in-string-p)
                     (or (gauche-paredit--in-regexp-p0)
                         (gauche-paredit--in-char-set-p0)))))
         (cond ((save-excursion
                  (paredit-handle-sexp-errors (progn (forward-sexp) t)
                    nil))
                (forward-char)
                (paredit-backward-delete-in-string))
               (t
                (message "Deleting spurious opening delimiter.")
                (delete-char +1))))
        (t
         (funcall f argument))))

(defun gauche-paredit-around-backward-delete (f &optional argument &rest _args)
  "around advice for `paredit-backward-delete'.

Handle backward deletion immediately after non-case-folding regexps.
If a regexp is empty, delete the whole regexp,
otherwise move backward into the regexp."
  (cond ((or (not gauche-paredit-mode)
             (consp argument)
             (integerp argument)
             (bobp))
         (funcall f argument))
        ;; #/../|
        ((and (eql ?\/ (char-before))
              (not (paredit-in-string-p))
              (save-excursion
                (backward-char)
                (gauche-paredit-in-regexp-p)))
         (cond ((save-excursion
                  (paredit-handle-sexp-errors (progn (backward-sexp) t)
                    nil))
                (backward-char)
                (paredit-forward-delete-in-string))
               (t
                (message "Deleting spurious closing delimiter.")
                (delete-char -1))))
        (t
         (funcall f argument))))

(defun gauche-paredit-around-forward-delete-in-string (f &rest args)
  "around advice for `paredit-forward-delete-in-string'.

Handle forward deletion inside regexps and char-sets.
Before a case-folding flag of a regexp, simply delete it.
Before a opening/closing delimiter, delete the whole expression
if the expression is empty, otherwise refuse to delete.
Before a POSIX chararcter set inside a char-set,
delete the character set.
Otherwise, simply delegate to `paredit-forward-delete-in-string'."
  (if (not gauche-paredit-mode)
      (apply f args)
    (let ((in-re (gauche-paredit--in-regexp-p0))
          (in-cs (gauche-paredit--in-char-set-p0)))
      (if (not (or in-re in-cs))
          (apply f args)
        (let* ((start+end (paredit-string-start+end-points))
               (beg (car start+end))
               (end (cdr start+end))
               (p (point)))
          (cond
           ;; #/../i
           ((and in-re (eql (char-after end) ?\i))
            (cond ((= p end)              ; #/../|i
                   (delete-char +1))
                  ((= p (1- end))         ; #/..|/i
                   (when (= p (1- end))   ; #/|/i
                     (delete-char -2)
                     (delete-char +2)))
                  ((= p (1+ beg))         ; #|/../i
                   (when (= p (- end 2))  ; #|//i
                     (delete-char -1)
                     (delete-char +3)))
                  (t
                   (apply f args))))
           ;; #/../, #[..]
           (t
            (cond ((= p end)              ; #/..|/
                   (when (= p (+ beg 2))  ; #/|/
                     (delete-char -2)
                     (delete-char +1)))
                  ((= p (1+ beg))         ; #|/../
                   (when (= p (1- end))   ; #|//
                     (delete-char -1)
                     (delete-char +2)))
                  ((and in-cs             ; #[..|[:..:]..]
                        (>= p (+ beg 2))
                        (not (paredit-in-string-escape-p))
                        (eql ?\[ (char-after p)))
                   (save-match-data
                     (goto-char p)
                     (re-search-forward (rx ":]") end t)
                     (delete-region p (match-end 0))))
                  (t
                   (apply f args))))))))))

(defun gauche-paredit-around-backward-delete-in-string (f &rest args)
  "around advice for `paredit-backward-delete-in-string'.

Handle backward deletion inside regexps and char-sets.
After a closing delimiter of a regexp, move backward into the regexp.
After a '#' prefix or a opening delimiter, delete the whole expression
if the expression is empty, otherwise refuse to delete.
After a POSIX chararcter set inside a char-set,
delete the character set.
Otherwise, simply delegate to `paredit-backward-delete-in-string'."
  (if (not gauche-paredit-mode)
      (apply f args)
    (let ((in-re (gauche-paredit--in-regexp-p0))
          (in-cs (gauche-paredit--in-char-set-p0)))
      (if (not (or in-re in-cs))
          (apply f args)
        (let* ((start+end (paredit-string-start+end-points))
               (beg (car start+end))
               (end (cdr start+end))
               (p (point)))
          (cond
           ;; #/../i
           ((and in-re (eql (char-after end) ?\i))
            (cond ((= p end)              ; #/../|i
                   (backward-char +1))
                  ((= p (+ beg 2))        ; #/|../i
                   (when (= p (1- end))   ; #/|/
                     (delete-char -2)
                     (delete-char +2)))
                  ((= p (1+ beg))         ; #|/../i
                   (when (= p (- end 2))  ; #|//i
                     (delete-char -1)
                     (delete-char +3)))
                  (t
                   (apply f args))))
           ;; #/../, #[..]
           (t
            (cond ((= p (+ beg 2))        ; #/|../
                   (when (= p end)        ; #/|/
                     (delete-char -2)
                     (delete-char +1)))
                  ((= p (1+ beg))         ; #|/../
                   (when (= p (1- end))   ; #|//
                     (delete-char -1)
                     (delete-char +2)))
                  ((and in-cs             ; #[..[:..:]|..]
                        (>= p (+ beg 2))
                        (not (paredit-in-string-escape-p))
                        (eql ?\] (char-before p)))
                   (save-match-data
                     (goto-char beg)
                     (let (pos)
                       (while (re-search-forward (rx "[:") p t)
                         (setq pos (match-beginning 0)))
                       (delete-region pos p))))
                  (t
                   (apply f args))))))))))

(defun gauche-paredit-around-reindent-defun (f &rest args)
  "around advice for `paredit-reindent-defun'.

A workaround for the issue where `gauche-mode--indent-lambda-formals'
does not work well in combination with `indent-sexp'."
  (if (or (not gauche-paredit-mode)
          (paredit-in-string-p)
          (paredit-in-comment-p))
      (apply f args)
    (save-excursion
      (let ((ep (progn (end-of-defun) (point))))
        (beginning-of-defun)
        (indent-region (point) ep)))))

(defvar gauche-paredit--advices
  `(
    (paredit-forward-delete
     :around
     ,#'gauche-paredit-around-forward-delete)
    (paredit-backward-delete
     :around
     ,#'gauche-paredit-around-backward-delete)
    (paredit-forward-delete-in-string
     :around
     ,#'gauche-paredit-around-forward-delete-in-string)
    (paredit-backward-delete-in-string
     :around
     ,#'gauche-paredit-around-backward-delete-in-string)
    (paredit-reindent-defun
     :around
     ,#'gauche-paredit-around-reindent-defun)
    ))

(defun gauche-paredit--setup-advices (enable)
  (cl-loop for (sym how fun) in gauche-paredit--advices
           do (if enable
                  (advice-add sym how fun)
                (advice-remove sym fun))))

(gauche-paredit--setup-advices t)

;;;###autoload
(defun enable-gauche-paredit-mode ()
  (interactive)
  (gauche-paredit-mode +1))

(defun disable-gauche-paredit-mode ()
  (interactive)
  (gauche-paredit-mode -1))

(provide 'gauche-paredit)
;; Local Variables:
;; comment-column: 42
;; End:
;;; gauche-paredit.el ends here
