;;; company-gauche.el --- do completion with company.el in gauche-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 OOHASHI Daichi

;; Author: OOHASHI Daichi <dico.leque.comicron@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'company)
(require 'gauche-mode)

(defun company-gauche-info (command &optional arg &rest ignore)
  (interactive (list 'interactive))
  (cl-case command
    (interactive
     (company-begin-backend 'company-gauche-info))
    (prefix
     (company-grab-symbol))
    (candidates
     (loop for c in (gauche-mode-info-candidates)
           when (string-prefix-p arg c)
           collect c))
    ))

;;;###autoload
(defun company-gauche-setup ()
  (setq-local company-backends `(company-gauche-info ,@company-backends)))

(provide 'company-gauche)
;;; company-gauche.el ends here
