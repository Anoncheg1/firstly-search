;;; dired-e.el --- Dired minor mode for fast navigation  -*- lexical-binding: t -*-

;; Copyright (c) 2024 Anoncheg1

;; Author: Anoncheg1
;; Keywords: matching, dired, isearch
;; URL: https://github.com/Anoncheg1/dired-e-mode
;; Version: 0.0.3
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
;; pressing alphabet or number key or any printable characters of
;; target filename or directory in current folder.  Are you still using
;; arrays?
;;
;; to activate, add lines to your Emacs configuration:
;; (require 'dired-e)
;; (add-hook 'dired-mode-hook #'dired-e-mode)
;;
;;; Code:

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

(defun dired-e--pre-command-hook-advice ()
  "Advice to add alphabet fast navigation to Dired mode."
  (let* ((key (this-single-command-keys))
         ;; (command (lookup-key global-map key nil))
         (key-char (key-description key)))
    (cond
     ;; activate navigation if printable character key was pressed
     ((and (not isearch-mode)
           (not (eq (string-match-p
                     dired-e-ignore-keys-re
                     key-char) 0))
           (eq (string-match-p "^[[:print:]]$" key-char) 0))
      (dired-isearch-filenames)
      isearch-regexp-function
      ;; from begining of word or not
      (setq isearch-regexp-function (if dired-e-from-begin
                                        #'dired-e--isearch-regexp-function
                                      #'word-search-regexp)) ; not from begining
      ;; suppress current command
      (setq this-command (lambda () (interactive) ()))
      ;; activate isearch by file name
      (setq isearch-string (key-description key))
      (setq isearch-message (key-description key))
      (setq isearch-success t isearch-adjusted 'toggle)
      ;; (isearch-update)
      (call-interactively 'isearch-repeat-forward))
     ;; ignore dired-special keys during isearch
     ((and isearch-mode
            (eq (string-match-p dired-e-ignore-keys-re key-char) 0))
      (setq this-command (lambda () (interactive) ()))
      (isearch-done)
      (isearch-clean-overlays))
     ;; speed up navigation
     ((and (eq last-command 'isearch-repeat-backward) (eq this-command 'isearch-repeat-forward))
      (call-interactively 'isearch-repeat-forward)) )))

;; rebind dired-mode-map - totally optional and may be nil
(defvar-keymap dired-e-mode-map
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

;;;###autoload
(define-minor-mode dired-e-mode
  "Alphabet fast navigation like dired-explorer."
  :lighter " dired-e" :global nil :group 'dired :version "28.2"
  (setq-local dired-isearch-filenames t)
  (setq-local isearch-wrap-pause 'no)

  (if dired-e-mode
      (add-hook 'pre-command-hook #'dired-e--pre-command-hook-advice nil t)
    (remove-hook 'pre-command-hook #'dired-e--pre-command-hook-advice t)))

;; -- fix that exit search and do other work
(keymap-unset isearch-mode-map "C-m")

;; (defun dired-e--isearch-exit-advice (&rest args)
;;   "Execute RET for Dired when in RET in isearch mode for exit."
;;   (execute-kbd-macro (kbd "RET")))

;; (advice-add 'isearch-exit :after #'dired-e--isearch-exit-advice)

;; -- Speed up navigation with navigation keys

(define-key isearch-mode-map "\C-p" #'isearch-repeat-backward)
(define-key isearch-mode-map "\C-n" #'isearch-repeat-forward)


(defun dired-e--my-goto-match-beginning ()
  "Place cursor always at the end."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))
(add-hook 'isearch-update-post-hook 'dired-e--my-goto-match-beginning)



(provide 'dired-e)
;;; dired-e.el ends here
