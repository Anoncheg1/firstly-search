;;; firstly-search-buffermenu.el --- Fast navigation for Package Menu mode -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg

;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: matching, dired, isearch
;; URL: https://codeberg.org/Anoncheg/firstly-search
;; Version: 0.1.3

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
;; Minor mode for `Buffer-menu-mode'.
;;
;; to activate, add lines to your Emacs configuration (Init file):
;; (require 'firstly-search-buffermenu)
;; (add-hook 'Buffer-menu-mode-hook #'firstly-search-buffermenu-mode)

(require 'firstly-search)
(require 'firstly-search-tabulated-list)

;;; Code:

(defgroup firstly-search-buffermenu nil
  "For major mode `Buffer-menu-mode'."
  :group 'firstly-search-buffermenu
  :prefix "firstly-search-buffermenu-")

(defcustom firstly-search-buffermenu-columns '((tabulated-list-column-name . "Buffer" ))
  "Non-nil means search in these columns."
  :local t
  :type 'sexp
  :group 'firstly-search-buffermenu)

(defcustom firstly-search-buffermenu-isearch-prefix "Buffer "
  "Regex for word search in package names."
  :local t
  :type 'string
  :group 'firstly-search-buffermenu)

(defcustom firstly-search-buffermenu-regex "\\(\\_<\\|*\\)"
  "Regex for word search in package names."
  :local t
  :type 'string
  :group 'firstly-search-buffermenu)

(defvar-keymap firstly-search-buffermenu-mode-map
  :doc "Replacement for `Buffer-menu-mode-map'."
  :parent firstly-search-tabulated-list-mode-map
  "RET"	#'Buffer-menu-this-window
  "M-0"	#'digit-argument
  "M-1"	#'Buffer-menu-1-window
  "M-2"	#'Buffer-menu-2-window
  ;; "M-3 .. 9"	#'digit-argument
  "M-?"	#'describe-mode
  "M-O"	#'Buffer-menu-view-other-window
  "M-S"	#'tabulated-list-sort
  "M-T"	#'Buffer-menu-toggle-files-only
  "M-U"	#'Buffer-menu-unmark-all
  "M-V"	#'Buffer-menu-view
  "M-b"	#'Buffer-menu-bury
  "M-d"	#'Buffer-menu-delete
  ;; "M-e .. f"	#'Buffer-menu-this-window
  "M-g"	#'revert-buffer
  "M-h"	#'describe-mode
  "M-k"	#'Buffer-menu-delete
  "M-m"	#'Buffer-menu-mark
  "M-n"	#'next-line
  "M-o"	#'Buffer-menu-other-window
  "M-p"	#'previous-line
  "M-q"	#'quit-window
  "M-s"	#'Buffer-menu-save
  "M-t"	#'Buffer-menu-visit-tags-table
  "M-u"	#'Buffer-menu-unmark
  "C-M-v"	#'Buffer-menu-select
  "C-M-x"	#'Buffer-menu-execute
  "M-{"	#'tabulated-list-narrow-current-column
  "M-}"	#'tabulated-list-widen-current-column)

;;;###autoload
(define-minor-mode firstly-search-buffermenu-mode
  "Instant search in package names.
Typing any printable character activate incremental search."
  :lighter " Fsearch"
  :global nil :group 'firstly-search-buffermenu
  (if firstly-search-buffermenu-mode
      (progn
        ;; (setq firstly-search-ignore-mode-map firstly-search-buffermenu-mode-map) ; ignore keys
        (setq firstly-search-isearch-prefix firstly-search-buffermenu-isearch-prefix)
        ;; search from the begining of the word or after "-" character.
        (setq firstly-search-regex firstly-search-buffermenu-regex)
        ;; (setq firstly-search-regex nil)
        ;; main isearch function to limit search to column, like dired-isearch-search-filenames
        (setq firstly-search-tabulated-list-columns firstly-search-buffermenu-columns)
        (setq firstly-search--isearch-search-fun-function #'firstly-search-tabulated-list--isearch-search-fun-function)
        (add-hook 'pre-command-hook #'firstly-search--pre-command-hook nil t) ; fast actication
        (add-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning nil t)) ; speed tweek
    (progn
      (remove-hook 'pre-command-hook #'firstly-search--pre-command-hook t)
      (remove-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning t))))


(provide 'firstly-search-buffermenu)
;;; firstly-search-buffermenu.el ends here
