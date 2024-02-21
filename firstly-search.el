;;; firstly-search.el --- Dired minor mode for fast navigation  -*- lexical-binding: t -*-

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
;; (require 'firstly-search)
;; (add-hook 'dired-mode-hook #'firstly-search-mode)
;;
;; Note:
;; C-n and C-p used during searching as C-s and C-r
;;
;; Many functions use text properties, to find properties use:
;;   M-: (print (text-properties-at (point)))
;;
;; How it works:
;;
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
;;
;;; Code:

(declare-function word-search-regexp "isearch")

(defgroup firstly-search nil
  "Name Matching."
  :group 'firstly-search
  :prefix "firstly-search-")

(defcustom firstly-search-ignore-keys-re "^[*%:.~#&=!]$"
  "Non-nil means apply this keys as Dired command not like name."
  :local t
  :type '(string)
  :group 'firstly-search)

(defcustom firstly-search-with-custom-regex t
  "Non-nil means search from begining of the word by default.
If non-nil `firstly-search-regex' used. "
  :local t
  :type 'boolean
  :group 'firstly-search)

(defcustom firstly-search-regex "\\_<" ; from begining or "\\(\\_<\\|-\\)"
  "Non-nil means search with this regex."
  :local t
  :type 'string
  :group 'firstly-search)

(defcustom firstly-search-isearch-prefix "filename "
  "Non-nil means search name from begining of word."
  :local t
  :type 'string
  :group 'firstly-search)

(defun firstly-search--isearch-regexp-function (string &optional lax)
  "Replacement for `isearch-regexp-function' to search by file name.
It looks for STRING from the begining of it.
Optional argument LAX not used."
  (setq lax lax) ; suppers Warning: Unused lexical argument `lax'
  (cond
   ((equal string "") "")
   (t  (concat firstly-search-regex string))))

(defvar-local firstly-search--isearch-navigation-flag nil
  "Non-nil means firstly-search navigation activated.
Allow to separate firstly-search navigation from isearch.
May be sub-minor-mode.")

(defvar-local firstly-search--saved-isearch-regexp-function nil
  "Save place.")

(defvar-local firstly-search--saved-isearch-wrap-pause nil
  "Save place.")

(defvar firstly-search--saved-isearch-mode-map nil
  "Save place.")

;; create copy of isearch-mode-map. Activates after typing.
(defvar-keymap firstly-search-nav-map
      :parent isearch-mode-map
      "C-p" #'isearch-repeat-backward
      "C-n" #'isearch-repeat-forward
      ;; "C-m" #'dired-find-file
      )


(defun firstly-search--isearch-change-map ()
  "Speed up navigation by rebinding active isearch keys."
  ;; (when firstly-search--isearch-navigation-flag
    ;; - fix that exit search and do other work
    (keymap-unset firstly-search-nav-map "C-m") ;; this do not modify original in fact
    ;; -- copy isearch map to create our replacement
    (setq firstly-search--saved-isearch-mode-map isearch-mode-map)
    (setq isearch-mode-map firstly-search-nav-map))


(defvar firstly-search-ignore-mode-map nil)

(defvar firstly-search--isearch-search-fun-function nil)

(defun firstly-search--isearch-mode-end-hook ()
  "Disable navigation."
  (when firstly-search--isearch-navigation-flag
    (setq firstly-search--isearch-navigation-flag nil) ;; called once
    ;; (print "restore")
    ;; restore isearch options
    (setopt isearch-wrap-pause firstly-search--saved-isearch-wrap-pause)
    (setq isearch-regexp-function firstly-search--saved-isearch-regexp-function)
    ;; attempt to clear our keymap modifications of isearch
    (setq isearch-mode-map firstly-search--saved-isearch-mode-map)
    ;; remove isearch advice
    (remove-function (local 'isearch-search-fun-function)
                     firstly-search--isearch-search-fun-function)
    (remove-hook 'isearch-mode-end-hook #'firstly-search--isearch-mode-end-hook t)))


(defun firstly-search--pre-command-hook-advice ()
  "Advice to add alphabet fast navigation to Dired mode."
  (let* ((key (this-single-command-keys))
         ;; (command (lookup-key global-map key nil))
         (key-char (key-description key)))
    (cond
     ;; - activate navigation if printable character key was pressed
     ((and (not isearch-mode)
           (not firstly-search--isearch-navigation-flag)
           (not (eq (string-match-p
                     firstly-search-ignore-keys-re
                     key-char) 0)) ; ignore some characters
           (eq (string-match-p "^[[:print:]]$" key-char) 0)
           ;; no command exist in dired-fs-mode-map - additional ignore some characters
           (not (commandp (lookup-key firstly-search-ignore-mode-map key nil))))

      ;; isearch activation
      (add-function :around (local 'isearch-search-fun-function)
                    firstly-search--isearch-search-fun-function
                    ;; add "filename "
                    (list (cons 'isearch-message-prefix firstly-search-isearch-prefix)))
      (setq firstly-search--isearch-navigation-flag t) ; separate navigation and isearch - flag
      (firstly-search--isearch-change-map) ;; modify isearch keys

      (setq firstly-search--saved-isearch-wrap-pause isearch-wrap-pause)
      (setopt isearch-wrap-pause 'no)
      ;; dired variant of activation of isearch
      ;; (funcall firstly-search--isearch-activation-function) ; (dired-isearch-filenames)
      (isearch-forward nil t)
      ;; (setq-local dired-isearch-filenames t)
      ;; (isearch-forward nil t)
      ;; from begining of word or not
      (setq firstly-search--saved-isearch-regexp-function isearch-regexp-function)
      (setq isearch-regexp-function (if firstly-search-with-custom-regex
                                        #'firstly-search--isearch-regexp-function
                                      #'word-search-regexp)) ; not from begining
      ;; activate isearch by file name
      (setq isearch-string (key-description key))
      (setq isearch-message (key-description key))
      (setq isearch-success t isearch-adjusted 'toggle)
      ;; replace current command
      (setq this-command #'isearch-repeat-forward) ; do nothing
      (add-hook 'isearch-mode-end-hook #'firstly-search--isearch-mode-end-hook nil t)
      )
     ;; - speed up navigation
     ((and firstly-search--isearch-navigation-flag
           (eq last-command #'isearch-repeat-backward)
           (eq this-command 'isearch-repeat-forward))
      (call-interactively #'isearch-repeat-forward)) )))


(defun firstly-search--my-goto-match-beginning ()
  "Place cursor always at the end of search result.
Used for speed up navingation."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))


;; rebind dired-mode-map - totally optional and may be nil
(defvar-keymap dired-fs-mode-map
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
  "C-M-v"       #'dired-view-file
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
  "C-M-O"       #'dired-do-chown ;; something special here
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
  "M-SPC"     #'dired-next-line
  "C-m" #'dired-find-file)


(defun firstly-search--check-same (str1 str2 str3)
  "Compare two string values both equal to 3 or not equal."
  (if (and (equal str1 str3) (equal str2 str3))
      t
    ;; else
    (if (and (not (equal str1 str3)) (not (equal str2 str3)))
          t
      ;; else
      nil)))


(defun firstly-search--next-single-property-change-by-value (property pos)
  "Return the position of next PROPERTY change by value.
Argument PROPERTY is in form (property . value).
Argument POS is (point) position."
  ;; TODO: require rewriting as `firstly-search--previous-single-property-change-by-value'
  ;; To test:
  ;; (goto-char (firstly-search--previous-single-property-change-by-value '(tabulated-list-column-name . "Package" ) (point)))
  (let ((pos1 pos)
        (prname (car property))
        (prvalue (cdr property)))
    ;; step to the next property
    (setq pos1 (next-single-property-change pos1 prname))
    ;; if current value is positive we just get next one
    ;; else look for positive
    (if (not (equal (get-text-property pos prname) prvalue))
        (while (and (not (eq pos1 nil)) (not (equal (get-text-property pos1 prname) prvalue)))
          (setq pos1 (next-single-property-change pos1 prname))
          ))
    ;; (print (get-text-property pos1 prname))
    ;; (print pos1)
    pos1))

(defun firstly-search--previous-single-property-change-by-value (property pos)
  "Return the position of next PROPERTY change by value.
Should behave like
Argument PROPERTY is in form (property . value).
Argument POS is (point) position."
  ;; case1: at the middle of text with property equal. we should go to the first char of text with property equal. (same)
  ;; case2: at the middle of text without property equal. we should go to the first char of text without property equal. (same)
  ;; case3: at the begining of text with property equal. we should go to the first char of text without property equal. (other)
  ;; case3: at the begining of text without property equal. we should go to the first char of text with property equal. (other)
  ;; To test:
  ;; (goto-char (previous-single-property-change (point) 'tabulated-list-column-name ))
  ;; (goto-char (firstly-search--previous-single-property-change-by-value '(tabulated-list-column-name . "Package" ) (point)))
  ;; (equal (get-text-property (point) 'tabulated-list-column-name) "Package")
  (let ((pos2 pos)
        (pos1 (previous-single-property-change pos (car property))) ; previous position
        (prname (car property))
        (prvalue (cdr property))
        (prvalue-cur (get-text-property pos (car property))) ; (get-text-property pos prname)
        prvalue-pos1 ; previous - one step back - value
        at-the-middle-flag)
    ;; (if (null pos1)
    ;;     (print (list"wtf" pos)))
    (when (not (null pos1))
      (setq prvalue-pos1 (get-text-property pos1 prname))
      (setq at-the-middle-flag (firstly-search--check-same prvalue-cur prvalue-pos1 prvalue))
      ;; (print (list prvalue-cur prvalue-pos1 prvalue))
      ;; (print (list "at-the-middle-flag" at-the-middle-flag))
      ;; (print (list "prvalue-pos1" prvalue-pos1))
      (if at-the-middle-flag
          (while (and (not (eq pos2 nil))
                      (firstly-search--check-same (get-text-property pos2 prname) prvalue-cur prvalue))
            (setq pos1 pos2)
            ;; (print pos1)
            (setq pos2 (previous-single-property-change pos1 prname)))
        ;; else at the edge
        (progn
          (setq pos2 pos1) ;; one step
          ;; (print (list (get-text-property pos2 prname) prvalue-cur prvalue (firstly-search--check-same (get-text-property pos2 prname) prvalue-cur prvalue)))
          (while (and (not (eq pos2 nil))
                      (not (firstly-search--check-same (get-text-property pos2 prname) prvalue-cur prvalue)))
            ;; (print (list "aa" (get-text-property pos2 prname) prvalue-cur prvalue (firstly-search--check-same (get-text-property pos2 prname) prvalue-cur prvalue)))
            (setq pos1 pos2)
            (setq pos2 (previous-single-property-change pos1 prname)))
        ))
      pos1)))


(defun firstly-search-fun-match-text-property (search-fun properties)
  "Return the function to search inside text that has the specified PROPERTIES.
The function will limit the search for matches only inside text
which has at least one of the text PROPERTIES wich in form of
list ((property . value) ...).  The argument SEARCH-FUN provides
the function to search text, and defaults to the value of
`isearch-search-fun-default' when nil.
Closely bound with `search-within-boundaries' behaviour."
  (setq properties (ensure-list properties))
  ;; (setq lexical-binding nil)
  ;; (seq-some (lambda (property) (print (car property))) properties)
  (apply-partially
   #'search-within-boundaries
   search-fun ; SEARCH-FUN
   ;; #'check-properties-at
   (lambda (pos) ; GET-FUN - check if point is on property
     (let ((pos (if isearch-forward pos (max (1- pos) (point-min))))) ;; if backward pos = pos - 1
        ;; (let ((ol (make-overlay pos (1+ pos))))
        ;;   (overlay-put ol 'face '(:background "dark red")))
       (seq-some (lambda (property)
                   ;; equal to value
                   (equal (get-text-property pos (car property)) (cdr property) )
                   ) ;; predicate for every property
                 properties)))
   (lambda (pos) ; NEXT-FUN - search for the next property.
     (let ((pos-list (if isearch-forward
                         (mapcar
                          (lambda (property)
                            (firstly-search--next-single-property-change-by-value property pos)) ; try to convert named function to closure with environment
                          properties)
                       ;; else backard
                       (mapcar
                        (lambda (property) ; should behave like previous-single-property-change
                          (firstly-search--previous-single-property-change-by-value property pos))
                        ;; (lambda (property)
                        ;;          (previous-single-property-change
                        ;;           pos (car property)))
                        properties)))) ; property-change
       (setq pos-list (delq nil pos-list)) ; remove nil
       (when pos-list (if isearch-forward
                          (seq-min pos-list) ; smallest
                        (seq-max pos-list)))))))





(provide 'firstly-search)
;;; firstly-search.el ends here