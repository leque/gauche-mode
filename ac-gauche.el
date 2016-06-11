;;; ac-gauche.el --- do completion with auto-complete.el in gauche-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 OOHASHI Daichi

;; Author: OOHASHI Daichi <dico.leque.comicron@gmail.com>
;; URL: https://github.com/leque/gauche-mode
;; Package-Requires: ((gauche-mode "0.1.0") (auto-complete "1.5.0"))
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

;;; (add-hook 'gauche-mode-hook #'ac-gauche-setup)

;;; Code:

(require 'auto-complete)
(require 'gauche-mode)

;;;###autoload
(defun ac-gauche-setup ()
  (setq ac-sources (append '(ac-source-gauche-info ac-source-gtags)
                           ac-sources)))

(ac-define-source gauche-info
                  '((symbol . "s")
                    (candidates . gauche-mode-info-candidates)
                    (cache)))

(add-to-list 'ac-modes 'gauche-mode)

(provide 'ac-gauche)
;;; ac-gauche.el ends here
