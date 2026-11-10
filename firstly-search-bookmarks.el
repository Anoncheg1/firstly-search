;;; firstly-search-bookmarks.el --- Fast navigation for Package Menu mode -*- lexical-binding: t -*-

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
;; Minor mode for `list-bookmarks' or `bookmark-bmenu-mode'.
;;
;; to activate, add lines to your Emacs configuration (Init file):
;; (require 'firstly-search-bookmarks)
;; (add-hook 'bookmark-bmenu-mode-hook #'firstly-search-bookmarks-mode)

(require 'firstly-search)
(require 'firstly-search-tabulated-list)
(require 'bookmark)

;;; Code:

(defgroup firstly-search-bookmarks nil
  "For major mode `bookmark-bmenu-mode' or `list-bookmarks'."
  :tag "Fast Bookmarks"
  :group 'firstly-search-bookmarks)

(defcustom firstly-search-bookmarks-columns '((tabulated-list-column-name . "Bookmark Name" ))
  "Non-nil means search in these columns."
  :type 'sexp
  :group 'firstly-search-bookmarks)

(defcustom firstly-search-bookmarks-isearch-prefix "B-Name "
  "Regex for word search in package names."
  :type 'string
  :group 'firstly-search-bookmarks)

(defcustom firstly-search-bookmarks-prefix "\\_<"
  "Regex for word search in package names."
  :type 'string
  :group 'firstly-search-bookmarks)


(defvar-keymap firstly-search-bookmarks-map
  :doc "Rebinding of keys that interfere with firstly search.
Replacement for `Buffer-menu-mode-map'.
Optional, may be nil.
It have bindings for commands that unset `firstly-search--change-map'.
When it have name `{major-mode}-map` it is visible and applied in
 parallel, that is why we use other name."
  :parent firstly-search-tabulated-list-mode-map
  "RET" #'bookmark-bmenu-this-window
  "M-v" #'bookmark-bmenu-select
  "M-w" #'bookmark-bmenu-locate
  "M-5" #'bookmark-bmenu-other-frame
  "M-2" #'bookmark-bmenu-2-window
  "M-1" #'bookmark-bmenu-1-window
  "M-j" #'bookmark-bmenu-this-window
  "M-f" #'bookmark-bmenu-this-window
  "M-o" #'bookmark-bmenu-other-window
  "M-s" #'bookmark-bmenu-save
  "M-k" #'bookmark-bmenu-delete
  "C-M-x" #'bookmark-bmenu-execute-deletions
  "M-d" #'bookmark-bmenu-delete
  "M-D" #'bookmark-bmenu-delete-all
  ;; "S-SPC" #'previous-line
  ;; "SPC" #'next-line
  ;; "DEL" #'bookmark-bmenu-backup-unmark
  "M-u" #'bookmark-bmenu-unmark
  "M-U" #'bookmark-bmenu-unmark-all
  "M-m" #'bookmark-bmenu-mark
  "M-M" #'bookmark-bmenu-mark-all
  "M-l" #'bookmark-bmenu-load
  "M-r" #'bookmark-bmenu-rename
  "M-R" #'bookmark-bmenu-relocate
  "M-t" #'bookmark-bmenu-toggle-filenames
  "M-a" #'bookmark-bmenu-show-annotation
  "M-A" #'bookmark-bmenu-show-all-annotations
  "M-E" #'bookmark-bmenu-edit-annotation
  "M-/" #'bookmark-bmenu-search)

;;;###autoload
(define-minor-mode firstly-search-bookmarks-mode
  "Instant search in package names.
Typing any printable character activate incremental search."
  :lighter " Fsearch"
  :global nil :group 'firstly-search-bookmarks
  (if firstly-search-bookmarks-mode
      (progn
        (setq-local firstly-search-isearch-prefix firstly-search-bookmarks-isearch-prefix)
        ;; search from the begining of the word or after "-" character.
        (setq-local firstly-search-regex-prefix firstly-search-bookmarks-prefix)
        ;; main isearch function to limit search to column, like dired-isearch-search-filenames
        (setq-local firstly-search-tabulated-list-columns firstly-search-bookmarks-columns)
        (setq-local firstly-search--isearch-search-fun-function #'firstly-search-tabulated-list--isearch-search-fun-function)
        (firstly-search--change-map firstly-search-bookmarks-map)
        (add-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning-hook nil t) ; for speed
        (add-hook 'pre-command-hook #'firstly-search--pre-command-hook nil t))
    (progn
      (remove-hook 'pre-command-hook #'firstly-search--pre-command-hook t)
      (remove-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning-hook t)
      (firstly-search--restore-map))))


(provide 'firstly-search-bookmarks)
;;; firstly-search-bookmarks.el ends here
