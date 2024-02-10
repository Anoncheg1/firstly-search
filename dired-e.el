;;; dired-e.el --- Dired minor mode for fast navigation  -*- lexical-binding: t -*-

;; Copyright (c) 2024 Anoncheg1

;; Author: Anoncheg1
;; Keywords: matching, dired, isearch
;; URL: https://github.com/Anoncheg1/dired-e-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

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
;; Dired minor mode to move cursor by just pressing alphabet or number
;; key or any printable characters.
;;
;; to activate isearch submodes add lines:
;; (require 'dired-e)
;; (add-hook 'dired-mode-hook #'dired-e-mode)
;;; Code:

(defun dired-e--isearch-regexp-function (string &optional lax)
  (cond
   ((equal string "") "")
   (t  (concat "^" string))))

;; (setq isearch-regexp-function #'word-search-regexp)
(setq isearch-regexp-function #'dired-e--isearch-regexp-function)

(defun dired-e--pre-command-hook-advice ()
  "Advice to add alphabet fast navigation to Dired mode."
  (let* ((key (this-single-command-keys))
	 ;; (main-event (aref key 0))
         )
        (when (and (not isearch-mode)
                   (eq (string-match-p "^[[:print:]]$" (key-description key)) 0))
          (dired-isearch-filenames)
          (setq this-command (lambda () (interactive) ()))
          ;; (setq this-command (lambda () (interactive) (setq isearch-string (key-description key)) (isearch-update)))
          (setq isearch-string (key-description key))
          (setq isearch-message (key-description key))
          (isearch-update))))

;;;###autoload
(define-minor-mode dired-e-mode
  "Alphabet fast navigation like dired-explorer."
  :lighter " dired-e" :global nil :group 'dired :version "28.2"
  (setq-local dired-isearch-filenames t)
  (setq-local isearch-wrap-pause 'no)

  (if dired-e-mode
      (add-hook 'pre-command-hook #'dired-e--pre-command-hook-advice nil t)
    (remove-hook 'pre-command-hook #'dired-e--pre-command-hook-advice t)))


(defun dired-e--isearch-exit-advice (&rest args)
  "Execute RET for Dired when in RET in isearch mode for exit."
  (execute-kbd-macro (kbd "RET")))

(advice-add 'isearch-exit :after #'dired-e--isearch-exit-advice)


(provide 'dired-e)
;;; dired-e.el ends here
