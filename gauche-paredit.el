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

(defvar gauche-paredit-paren-prefix-pat
  (rx
   (or
    (seq "#" (+ digit) "=")                      ; R7RS datum labels
    (seq "#" (any "su") (or "8" "16" "32" "64")) ; R7RS bytevectors + SRFI-4
    (seq "#" (any "f") (or "16" "32" "64"))      ; SRFI-4 + Gauche's extension
    "#vu8"                                       ; R6RS bytevectors
    "(^"                                         ; (^(x y) ...)
    "#?="                                        ; debug-print
    )))

(defun gauche-paredit-space-for-delimiter-p (endp delimiter)
  (or endp
      (if (= (char-syntax delimiter) ?\()
          (not (looking-back gauche-paredit-paren-prefix-pat))
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
  "After `#`, insert pair of slashes.
At the end of a regexp, move past the closing slash.
In the middle of regexp, insert a backslash-escaped slash.
Otherwise, insert a literal slash.
"
  (interactive "P")
  (cond ((gauche-paredit-in-regexp-p)
         (let* ((pair (paredit-string-start+end-points))
                (start (car pair))
                (end (cdr pair))
                (pos (point)))
           (cond
            ;; before an open/close slash
            ((or (= pos (1+ start))
                 (= pos (if (= ?\/ (char-after end))
                            end
                          (1- end))))
             (forward-char))
            ;; before `i' flag
            ((= pos end)
             (insert ?\/))
            (t
             (insert ?\\ ?\/)))))
        ((paredit-in-comment-p)
         (insert ?\/))
        ((not (paredit-in-char-p))
         (if (= (char-before) ?\#)
             (paredit-insert-pair n ?\/ ?\/ 'paredit-forward-for-quote)
           (insert ?\/)))))

(defun gauche-paredit-open-square (&optional n)
  (interactive "P")
  (if (not (gauche-paredit-in-char-set-p))
      (paredit-open-square n)
    (let ((p (paredit-string-start+end-points)))
      (if (= (point) (1+ (car p)))
          (forward-char)
        (let ((ch (read-char)))
          (if (eql ch ?\:)
              (let ((name (completing-read
                           "char-set name: "
                           gauche-mode-posix-char-set-names)))
                (insert "[:" name ":]"))
            (insert "\\["
                    (if (eql ch ?\ )
                        ""
                      ch))))))))

(defun gauche-paredit-close-square (&optional n)
  (interactive "P")
  (if (not (gauche-paredit-in-char-set-p))
      (paredit-close-square n)
    (let ((p (paredit-string-start+end-points)))
      (if (= (point) (cdr p))
          (forward-char)
        (insert "\\]")))))

(defun gauche-paredit-around-forward-delete-in-string (f &rest args)
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

(advice-add 'paredit-forward-delete-in-string
            :around #'gauche-paredit-around-forward-delete-in-string)

(advice-add 'paredit-backward-delete-in-string
            :around #'gauche-paredit-around-backward-delete-in-string)

(defvar gauche-paredit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "/") #'gauche-paredit-slash)
    (define-key map (kbd "[") #'gauche-paredit-open-square)
    (define-key map (kbd "]") #'gauche-paredit-close-square)
    map))

;;;###autoload
(define-minor-mode gauche-paredit-mode
  "Minor-mode for Gauche-aware paredit"
  nil
  gauche-paredit-mode-map
  (setq-local paredit-space-for-delimiter-predicates
              (list #'gauche-paredit-space-for-delimiter-p))
  (paredit-mode (if gauche-paredit-mode +1 -1)))

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
