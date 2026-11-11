;;; firstly-search-package.el --- Fast navigation for Package Menu mode -*- lexical-binding: t -*-

;; Copyright (c) 2024-2026 github.com/Anoncheg1,codeberg.org/Anoncheg

;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: matching, dired, isearch
;; URL: https://codeberg.org/Anoncheg/firstly-search
;; Version: 0.2
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; License

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
;; Minor mode for `package-menu-mode'.
;;
;; to activate, add lines to your Emacs configuration:
;; (require 'firstly-search-package)
;; (add-hook 'package-menu-mode-hook #'firstly-search-package-mode)

(require 'firstly-search)
(require 'firstly-search-tabulated-list)
(require 'package)

;;; Code:

(defgroup firstly-search-package nil
  "For major mode `package-menu-mode'."
  :tag "Fast Package menu"
  :group 'firstly-search-package)

(defcustom firstly-search-package-columns '((tabulated-list-column-name . "Package" ))
  "Non-nil means search in these columns."
  :type 'sexp
  :group 'firstly-search-package)

(defcustom firstly-search-package-isearch-prefix "Package "
  "Regex for word search in package names."
  :type 'string
  :group 'firstly-search-package)

(defcustom firstly-search-package-prefix "\\(\\_<\\|-\\)"
  "Regex for word search in package names."
  :type 'string
  :group 'firstly-search-package)

(defvar-keymap firstly-search-package-map
  :doc "Rebinding of keys that interfere with firstly search.
Replacement for `package-menu-mode-map'.
Optional, may be nil.
It have bindings for commands that unset `firstly-search--change-map'.
When it have name `{major-mode}-map` it is visible and applied in
 parallel, that is why we use other name."
  :parent firstly-search-tabulated-list-mode-map
  ;; "-"	#'negative-argument
  ;; "0 .. 9"	#'digit-argument
  ;; "M-H"	#'package-menu-hide-package
  ;; "M-S"	#'tabulated-list-sort
  ;; "M-U"	#'package-menu-mark-upgrades
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
  "W"		#'package-browse-url
  "X"		#'package-menu-execute)

;;;###autoload
(define-minor-mode firstly-search-package-mode
  "Instant search in package names.
Typing any printable character activate incremental search."
  :lighter " Fsearch"
  :global nil :group 'firstly-search-package
  (if firstly-search-package-mode
      (progn
        (setq-local firstly-search-isearch-prefix firstly-search-package-isearch-prefix)
        ;; search from the begining of the word or after "-" character.
        (setq-local firstly-search-regex-prefix firstly-search-package-prefix)
        ;; main isearch function to limit search to column, like dired-isearch-search-filenames
        (setq-local firstly-search-tabulated-list-columns firstly-search-package-columns)
        (setq-local firstly-search--isearch-search-fun-function #'firstly-search-tabulated-list--isearch-search-fun-function)
        (firstly-search--change-map firstly-search-package-map)
        (add-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning-hook nil t) ; for speed
        (add-hook 'pre-command-hook #'firstly-search--pre-command-hook nil t)) ; speed jumping
    (progn
      (remove-hook 'pre-command-hook #'firstly-search--pre-command-hook t)
      (remove-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning-hook t)
      (firstly-search--restore-map))))


(provide 'firstly-search-package)
;;; firstly-search-package.el ends here
