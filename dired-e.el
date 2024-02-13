;;; dired-e.el --- Dired minor mode for fast navigation  -*- lexical-binding: t -*-

;; Copyright (c) 2024 Anoncheg1

;; Author: Anoncheg1
;; Keywords: matching, dired, isearch
;; URL: https://github.com/Anoncheg1/dired-e
;; Version: 0.0.5
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
;;
;; to activate, add lines to your Emacs configuration:
;; (require 'dired-e)
;; (add-hook 'dired-mode-hook #'dired-e-mode)
;;
;; Note:
;; C-n and C-p used during searching as C-s and C-r
;;
;;; Code:

(require 'dired)

(declare-function word-search-regexp "isearch")

(defgroup dired-e nil
  "Name Matching."
  :group 'dired-e
  :prefix "dired-e-")

(defcustom dired-e-ignore-keys-re "^[*%:.~#&=!]$"
  "Non-nil means apply this keys as Dired command not like name."
  :local t
  :type '(string)
  :group 'dired-e)

(defcustom dired-e-from-begin t
  "Non-nil means search name from begining of word."
  :local t
  :type 'boolean
  :group 'dired-e)

(defun dired-e--isearch-regexp-function (string &optional lax)
  "Replacement for `isearch-regexp-function' to search by file name.
It looks for STRING from the begining of it.
Optional argument LAX not used."
  (setq lax lax) ; suppers Warning: Unused lexical argument `lax'
  (cond
   ((equal string "") "")
   (t  (concat "\\_<" string)))) ;; from begining


(defvar-local dired-e--isearch-navigation-flag nil
  "Allow to separate dired-e navigation from isearch.
May be sub-minor-mode.")

;; (defun dired-e--ignore-dired-advanced-keys-durion-navigation()
;;   (interactive)
;;   (isearch-done)
;;   (isearch-clean-overlays))

(defvar-local dired-e--saved-isearch-regexp-function nil)
(defvar-local dired-e--saved-isearch-wrap-pause nil)

(defun dired-e--pre-command-hook-advice ()
  "Advice to add alphabet fast navigation to Dired mode."
  (let* ((key (this-single-command-keys))
         ;; (command (lookup-key global-map key nil))
         (key-char (key-description key)))
    (cond
     ;; - activate navigation if printable character key was pressed
     ((and (not isearch-mode)
           (not dired-e--isearch-navigation-flag)
           (not (eq (string-match-p
                     dired-e-ignore-keys-re
                     key-char) 0))
           (eq (string-match-p "^[[:print:]]$" key-char) 0))
      ;; isearch activation
      ;; (setq-local dired-isearch-filenames t)
      (setq dired-e--saved-isearch-wrap-pause isearch-wrap-pause)
      (setopt isearch-wrap-pause 'no)
      (dired-isearch-filenames)
      ;; from begining of word or not
      (setq dired-e--saved-isearch-regexp-function isearch-regexp-function)
      (setq isearch-regexp-function (if dired-e-from-begin
                                        #'dired-e--isearch-regexp-function
                                      #'word-search-regexp)) ; not from begining
      ;; activate isearch by file name
      (setq isearch-string (key-description key))
      (setq isearch-message (key-description key))
      (setq isearch-success t isearch-adjusted 'toggle)
      (setq dired-e--isearch-navigation-flag t) ; separate navigation from isearch flag
      ;; replace current command
      (setq this-command #'isearch-repeat-forward) ; do nothing

      )
     ;; ;; - disable navigation and ignore dired-special keys during isearch
     ;; ((and dired-e--isearch-navigation-flag ; isearch-mode
     ;;        (eq (string-match-p dired-e-ignore-keys-re key-char) 0))

     ;;  (setq this-command (lambda () (interactive) ())) ;; do nothing
     ;;  ;; (setq this-command #'dired-e--ignore-dired-advanced-keys-durion-navigation) ; do nothing
     ;;  (setq dired-e--isearch-navigation-flag nil)
     ;;  (isearch-done)
     ;;  (isearch-clean-overlays)
     ;;  )
     ;; - speed up navigation
     ((and dired-e--isearch-navigation-flag
           (eq last-command #'isearch-repeat-backward)
           (eq this-command 'isearch-repeat-forward))
      (call-interactively #'isearch-repeat-forward)) )))

;; rebind dired-mode-map - totally optional and may be nil
(defvar-keymap dired-e-mode-map
  ;; -- standard dired
  "M-a"       #'dired-find-alternate-file
  "M-d"       #'dired-flag-file-deletion
  "M-e"       #'dired-find-file
  "M-f"       #'dired-find-file
  ;; "C-m"     #'dired-find-file
  "M-g"       #'revert-buffer
  "M-i"       #'dired-maybe-insert-subdir
  "M-j"       #'dired-goto-file
  "M-k"       #'dired-do-kill-lines
  "M-l"       #'dired-do-redisplay
  "M-m"       #'dired-mark
  "M-n"       #'dired-next-line
  "M-o"       #'dired-find-file-other-window
  ;; "C-o"     #'dired-display-file
  "M-p"       #'dired-previous-line
  "M-s"       #'dired-sort-toggle-or-edit
  "M-t"       #'dired-toggle-marks
  "M-u"       #'dired-unmark
  "M-v"       #'dired-view-file
  "M-w"       #'dired-copy-filename-as-kill
  "M-W"       #'browse-url-of-dired-file
  "C-M-x"       #'dired-do-flagged-delete
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
  "M-O"       #'dired-do-chown
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

(defun dired-e--my-goto-match-beginning ()
  "Place cursor always at the end."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defun dired-e--isearch-change-map ()
  "Speed up navigation by rebinding isearch keys."
  ;; -- fix that exit search and do other work
  (keymap-unset overriding-terminal-local-map "C-m")
  ;; -- Speed up navigation with navigation keys
  (define-key overriding-terminal-local-map "\C-p" #'isearch-repeat-backward)
  (define-key overriding-terminal-local-map "\C-n" #'isearch-repeat-forward)
  )

(defun dired-e--isearch-mode-end-hook ()
  "Disable navigation."
  (when dired-e--isearch-navigation-flag
    (setq dired-e--isearch-navigation-flag nil)
    ;; restore isearch options
    (setopt isearch-wrap-pause dired-e--saved-isearch-wrap-pause)
    (setq isearch-regexp-function dired-e--saved-isearch-regexp-function)
    ;; attempt to clear our keymap modifications of isearch
    (setq overriding-terminal-local-map nil)))

;;;###autoload
(define-minor-mode dired-e-mode
  "Alphabet fast navigation like dired-explorer."
  :lighter " dired-e"
  :global nil :group 'dired :version "28.2"
  (if dired-e-mode
      (progn
        (add-hook 'pre-command-hook #'dired-e--pre-command-hook-advice nil t)
        (add-hook 'isearch-update-post-hook #'dired-e--my-goto-match-beginning nil t)
        (add-hook 'isearch-mode-hook #'dired-e--isearch-change-map nil t)
        (add-hook 'isearch-mode-end-hook #'dired-e--isearch-mode-end-hook nil t))
    (progn
      (remove-hook 'pre-command-hook #'dired-e--pre-command-hook-advice t)
      (remove-hook 'isearch-update-post-hook #'dired-e--my-goto-match-beginning t)
      (remove-hook 'isearch-mode-hook #'dired-e--isearch-change-map t))))


(provide 'dired-e)
;;; dired-e.el ends here
