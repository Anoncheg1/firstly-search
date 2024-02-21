;;; package-fs.el --- package-menu-mode minor mode for fast navigation  -*- lexical-binding: t -*-

;; Copyright (c) 2024 Anoncheg1

;; Author: Anoncheg1
;; Keywords: matching, dired, isearch
;; URL: https://github.com/Anoncheg1/firstly-search
;; Version: 0.0.7
;; Package-Requires: ((emacs "29.1"))

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
;; Modern way of navigation.  Dired minor mode to move cursor by just
;; pressing any printable characters of target filename or directory
;; in current folder.  Are you still using arrays?
;; Old dired-explorer.el package do the same.
;;
;; to activate, add lines to your Emacs configuration:
;; (require 'package-fs)
;; (add-hook 'package-menu-mode-hook #'package-fs-mode)

(require 'firstly-search)

(defgroup package-fs nil
  "For major mode `package-menu-mode'."
  :group 'package-fs
  :prefix "package-fs-")

(defcustom package-fs-columns '((tabulated-list-column-name . "Package" ))
  "Non-nil means search in these columns."
  :local t
  :type '(string)
  :group 'package-fs)

(defun package-fs--isearch-search-fun-function (orig-fun)
  "`isearch-mode-hook'."
  (firstly-search-fun-match-text-property
   (funcall orig-fun) package-fs-columns))

(defvar-keymap package-fs-mode-map
  "-"		#'negative-argument
  ;; "0 .. 9"	digit-argument
  ;; ?		package-menu-describe-package
  "M-H"	#'package-menu-hide-package
  "M-S"	#'tabulated-list-sort
  "M-U"	#'package-menu-mark-upgrades
  "M-b"	#'package-report-bug
  "M-d"	#'package-menu-mark-delete
  "M-g"	#'revert-buffer
  "M-h"	#'package-menu-quick-help
  "M-i"	#'package-menu-mark-install
  "M-n"	#'next-line
  "M-p"	#'previous-line
  "M-q"	#'quit-window
  "M-r"	#'revert-buffer
  "M-u"	#'package-menu-mark-unmark
  "M-w"	#'package-browse-url
  "M-x"	#'package-menu-execute
)

;;;###autoload
(define-minor-mode package-fs-mode
  "Instant search in package names.
Typing any printable character activate incremental search."
  :lighter " p-fs"
  :global nil :group 'package-fs
  (if package-fs-mode
      (progn
        (setq firstly-search-ignore-mode-map package-fs-mode-map) ; ignore keys
        (setq firstly-search-isearch-prefix "Package ")
        ;; search from the begining of the word or after "-" character.
        (setq firstly-search-regex "\\(\\_<\\|-\\)")
        (setq firstly-search--isearch-search-fun-function #'package-fs--isearch-search-fun-function) ; dired-isearch-search-filenames
        (add-hook 'pre-command-hook #'firstly-search--pre-command-hook-advice nil t) ; fast actication
        (add-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning nil t)) ; speed tweek
    (progn
      ;; (remove-hook 'isearch-mode-end-hook #'package-fs--isearch-mode-hook t)
      (remove-hook 'pre-command-hook #'firstly-search--pre-command-hook-advice t)
      (remove-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning t)))))


(provide 'package-fs)
;;; package-fs.el ends here
