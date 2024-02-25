;;; firstly-search-dired.el --- Dired minor mode for fast navigation  -*- lexical-binding: t -*-

;; Copyright (c) 2024 Anoncheg1

;; Author: Anoncheg1
;; Keywords: matching, dired, isearch
;; URL: https://github.com/Anoncheg1/firstly-search
;; Version: 0.0.9

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

;; to activate, add lines to your Emacs configuration:
;; (require 'firstly-search-dired)
;; (add-hook 'dired-mode-hook #'firstly-search-dired-mode)

;; C-m or RET may be used to fast choose item.

;; Note:
;; C-n and C-p used during searching as C-s and C-r

;; Many functions use text properties, to find properties use:
;;   M-: (print (text-properties-at (point)))

;; How it works:

;; `dired-mode' add `dired-isearch-filenames-setup' to
;; `isearch-mode-hook', that activate `dired-isearch-filenames-mode'
;; which add advice to isearch to search in filenames when isearch
;; started with `dired-isearch-filenames' variable.  We replace
;; `isearch-regexp-function' that search string in filename and just
;; call isearch with `dired-isearch-filenames'.
;; `isearch-search-fun-function' replaced with
;; `dired-isearch-search-filenames' that wrap ? with
;; `isearch-search-fun-in-text-property' that search in text wthat
;; have properties `dired-filename' and `dired-symlink-filename'.

;;; Code:

(require 'dired-aux)
(require 'firstly-search)

;; totally optional and may be nil
(defvar-keymap firstly-search-dired-mode-map
  :doc "Replacement for `dired-mode-map'.
Totally optional and may be nil."
  ;; special-mode-map
  "M-?"	#'describe-mode
  "M-h"	#'describe-mode
  ;; ">" #'end-of-buffer
  ;; "<" #'beginning-of-buffer
  "M-g"	#'revert-buffer
  ;; -- standard dired
  "M-a"       #'dired-find-alternate-file
  "M-d"       #'dired-flag-file-deletion
  "M-e"       #'dired-find-file
  "M-f"       #'dired-find-file
  "C-m"       #'dired-find-file
  ;; "M-g"       #'revert-buffer
  "M-i"       #'dired-maybe-insert-subdir
  "M-j"       #'dired-goto-file
  "M-k"       #'dired-do-kill-lines
  "M-l"       #'dired-do-redisplay
  "M-m"       #'dired-mark
  ;; "M-n"       #'dired-next-line
  "M-o"       #'dired-find-file-other-window
  ;; "C-o"     #'dired-display-file
  ;; "M-p"       #'dired-previous-line
  "M-s"       #'dired-sort-toggle-or-edit
  "M-t"       #'dired-toggle-marks
  "M-u"       #'dired-unmark
  "C-M-v"     #'dired-view-file
  "M-w"       #'dired-copy-filename-as-kill
  "M-W"       #'browse-url-of-dired-file
  "C-M-x"     #'dired-do-flagged-delete
  "M-y"       #'dired-show-file-type
  "M-+"       #'dired-create-directory
  "M-A"       #'dired-do-find-regexp
  "M-C"       #'dired-do-copy
  "M-B"       #'dired-do-byte-compile
  "M-D"       #'dired-do-delete
  "M-G"       #'dired-do-chgrp
  "M-H"       #'dired-do-hardlink
  "M-I"       #'dired-do-info
  "M-L"       #'dired-do-load
  "M-M"       #'dired-do-chmod
  "M-N"       #'dired-do-man
  "C-M-O"     #'dired-do-chown ;; something special here
  "M-P"       #'dired-do-print
  "M-Q"       #'dired-do-find-regexp-and-replace
  "M-R"       #'dired-do-rename
  "M-S"       #'dired-do-symlink
  "M-T"       #'dired-do-touch
  "M-X"       #'dired-do-shell-command
  "M-Y"       #'dired-do-relsymlink
  "M-Z"       #'dired-do-compress
  "M-c"       #'dired-do-compress-to
  "M-U"       #'dired-unmark-all-marks
  ;; "M-<"       #'dired-prev-dirline
  ;; "M->"       #'dired-next-dirline
  "M-^"       #'dired-up-directory
  "M-SPC"     #'dired-next-line)

;;;###autoload
(define-minor-mode firstly-search-dired-mode
  "Instant search in file names.
Typing any printable character activate incremental search."
  :lighter " Fsearch"
  :global nil :group 'firstly-search
  (if firstly-search-dired-mode
      (progn
        (setq firstly-search-ignore-mode-map firstly-search-dired-mode-map) ; ignore keys
        (setq firstly-search--isearch-search-fun-function #'dired-isearch-search-filenames)
        (add-hook 'pre-command-hook #'firstly-search--pre-command-hook nil t) ; fast actication
        (add-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning nil t) ; speed tweek
        )
    (progn
      (remove-hook 'pre-command-hook #'firstly-search--pre-command-hook t)
      (remove-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning t))))


(provide 'firstly-search-dired)
;;; firstly-search-dired.el ends here
