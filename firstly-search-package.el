;;; firstly-search-package.el --- Fast navigation for Package Menu mode -*- lexical-binding: t -*-

;; Copyright (c) 2024 Anoncheg1

;; Author: Anoncheg1
;; Keywords: matching, dired, isearch
;; URL: https://github.com/Anoncheg1/firstly-search
;; Version: 0.0.8

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
;; (require 'firstly-search-package)
;; (add-hook 'package-menu-mode-hook #'firstly-search-package-mode)

(require 'firstly-search)

;;; Code:

(defgroup firstly-search-package nil
  "For major mode `package-menu-mode'."
  :group 'firstly-search-package
  :prefix "firstly-search-package-")

(defcustom firstly-search-package-columns '((tabulated-list-column-name . "Package" ))
  "Non-nil means search in these columns."
  :local t
  :type 'sexp
  :group 'firstly-search-package)

(defcustom firstly-search-package-isearch-prefix " Package"
  "Regex for word search in package names."
  :local t
  :type 'string
  :group 'firstly-search-package)


(defcustom firstly-search-package-regex "\\(\\_<\\|-\\)"
  "Regex for word search in package names."
  :local t
  :type 'string
  :group 'firstly-search-package)

(defun firstly-search-package--isearch-search-fun-function (orig-fun)
  "Replacement for `isearch-search-fun-function'.
This function limit search to desired columns.
Argument ORIG-FUN isearch internal function."
  (firstly-search-fun-match-text-property
   (funcall orig-fun) firstly-search-package-columns))

(defvar-keymap firstly-search-package-mode-map
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
  "C-M-w"	#'package-browse-url
  "C-M-x"	#'package-menu-execute)

;;;###autoload
(define-minor-mode firstly-search-package-mode
  "Instant search in package names.
Typing any printable character activate incremental search."
  :lighter " Fsearch"
  :global nil :group 'firstly-search-package
  (if firstly-search-package-mode
      (progn
        (setq firstly-search-ignore-mode-map firstly-search-package-mode-map) ; ignore keys
        (setq firstly-search-isearch-prefix firstly-search-package-isearch-prefix)
        ;; search from the begining of the word or after "-" character.
        (setq firstly-search-regex firstly-search-package-regex)
        ;; main isearch function to limit search to column, like dired-isearch-search-filenames
        (setq firstly-search--isearch-search-fun-function #'firstly-search-package--isearch-search-fun-function)
        (add-hook 'pre-command-hook #'firstly-search--pre-command-hook-advice nil t) ; fast actication
        (add-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning nil t)) ; speed tweek
    (progn
      (remove-hook 'pre-command-hook #'firstly-search--pre-command-hook-advice t)
      (remove-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning t))))


(provide 'firstly-search-package)
;;; firstly-search-package.el ends here
