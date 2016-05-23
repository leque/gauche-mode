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

;;; Code:
(require 'rx)
(require 'paredit)
(require 'gauche-mode)

(defvar gauche-paredit-paren-prefix-pat
  (rx
   (or
    (seq "#" (+ digit) "=")                     ; R7RS datum labels
    (seq "#" (in "su") (or "8" "16" "32" "64")) ; R7RS bytevectors + SRFI-34
    (seq "#f" (or "16" "32" "64"))              ; SRFI-34
    "#vu8"                                      ; R6RS bytevectors
    "(^"                                        ; (^(x y) ...)
    "#?="                                       ; debug-print
    )))

(defun gauche-paredit-space-for-delimiter-p (endp delimiter)
  (or endp
      (if (= (char-syntax delimiter) ?\()
          (not (looking-back gauche-paredit-paren-prefix-pat))
        t)))

(defun gauche-paredit-in-regexp-p ()
  (and (paredit-in-string-p)
       (let ((start (car (paredit-string-start+end-points))))
         (string= "#/"
                  (buffer-substring-no-properties start
                                                  (+ 2 start))))))

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
           (if (or (= pos
                      (if (= ?\/ (char-after end))
                          end
                        (1- end)))
                   (= pos
                      (1+ start)))
               (forward-char)
             (if (= pos end)
                 (insert ?\/)
               (insert ?\\ ?\/)))))
        ((paredit-in-comment-p)
         (insert ?\/))
        ((not (paredit-in-char-p))
         (if (= (char-before) ?\#)
             (paredit-insert-pair n ?\/ ?\/ 'paredit-forward-for-quote)
           (insert ?\/)))))

(add-hook 'gauche-mode-hook
          #'(lambda ()
              (set (make-local-variable
                    'paredit-space-for-delimiter-predicates)
                   (list #'gauche-paredit-space-for-delimiter-p))))

(define-key gauche-mode-map "/" #'gauche-paredit-slash)

(provide 'gauche-paredit)
;;; gauche-paredit.el ends here
