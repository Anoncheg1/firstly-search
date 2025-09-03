;;; firstly-search.el --- Search with any key: Dired, Package, Buffer menu modes  -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: matching, isearch, navigation, dired, packagemenu
;; URL: https://codeberg.org/Anoncheg/firstly-search
;; Created: 10 Sep 2024
;; Version: 0.1.4
;; Package-Requires: ((emacs "29.1") (compat "30.1"))

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

;; Any key activate search.
;; There are minor modes for major modes: Dired, Package Menu.
;; Cursor is moved by just pressing any printable characters
;; of target filename or directory in current folder.

;; Activation:
;; - for Dired:	M-x firstly-search-dired-mode RET
;; - for Package Menu:	M-x firstly-search-package-mode RET
;; - for Buffer Menu:	M-x firstly-search-buffermenu-mode RET
;; - for Bookmarks:	M-x firstly-search-bookmarks-mode RET

;; in Emacs configuration (Init file):
;; (require 'firstly-search-dired)
;; (require 'firstly-search-package)
;; (require 'firstly-search-buffermenu)
;; (require 'firstly-search-bookmarks)
;; (add-hook 'dired-mode-hook #'firstly-search-dired-mode)
;; (add-hook 'package-menu-mode-hook #'firstly-search-package-mode)
;; (add-hook 'Buffer-menu-mode-hook #'firstly-search-buffermenu-mode)
;; (add-hook 'bookmark-bmenu-mode-hook #'firstly-search-bookmarks-mode)

;; Customization:

;; M-x customize-group RET firstly-search
;; M-x customize-group RET firstly-search-dired
;; M-x customize-group RET firstly-search-package
;; M-x customize-group RET firstly-search-buffermenu
;; M-x customize-group RET firstly-search-bookmarks

;; Note:
;; C-n and C-p is used during searching as C-s and C-r

;; How it works:

;; We use `pre-command-hook' called for any key pressed, if key is
;; simple and not in "ignore-keys" we activate incremental search with
;; modified `isearch-search-fun-function' that limit search to bounds
;; in buffer and some other tweeks.

;; For Dired default `isearch-search-fun-in-text-property' function is
;; used for search and only in text which have not nil specified "text
;; properties".

;; For modes  based on tabulated-list  (Buffer Menu, Package  menu) we
;; create  modified version,  `firstly-search-fun-match-text-property'
;; function that search  only in text which  have specified properties
;; with specified values.

;; Many functions use text properties, to investigate them use:
;;   M-: (print (text-properties-at (point)))
;;
;; Files:
;; - first-search.el - common functionality for other files.
;; - first-search-{dired,package,buffermenu,bookmarks}.el -
;;   define user available minor modes for specific major modes.

;;; Code:

(declare-function word-search-regexp "isearch")

(defgroup firstly-search nil
  "Search with any key."
  :group 'firstly-search
  :prefix "firstly-search-")

(defcustom firstly-search-ignore-keys-re "^[*%:.~#&=!]$"
  "Non-nil means apply ignore this keys, for mode specific commands."
  :local t
  :type '(string)
  :group 'firstly-search)

(defcustom firstly-search-regex "\\_<"
  "Non-nil means search with this regex.
This value used to search from begining of every word separated
by space. You may use \\(\\_<\\|-\\) for search in words
separated by -."
  :local t
  :type 'string
  :group 'firstly-search)

(defcustom firstly-search-isearch-prefix "filename "
  "Non-nil means add string to isearch prompt."
  :local t
  :type 'string
  :group 'firstly-search)

(defvar firstly-search--isearch-navigation-flag nil
  "Non-nil means firstly-search navigation activated.
Allow to separate firstly-search navigation from isearch.
May be sub-minor-mode.")

(defvar-local firstly-search--saved-isearch-regexp-function nil
  "Place for temporarely store isearch previous settings.")

(defvar-local firstly-search--saved-isearch-wrap-pause nil
  "Place for temporarely store isearch previous settings.")

(defvar firstly-search--saved-isearch-mode-map nil
  "Place for temporarely store isearch previous settings.")

(defvar-local firstly-search-ignore-mode-map nil)

(defvar-local firstly-search--isearch-search-fun-function nil)


(defun firstly-search--isearch-regexp-function (string &optional lax)
  "Replacement for `isearch-regexp-function' to search by file name.
It looks for STRING from the begining of it.
Optional argument LAX not used."
  (setq lax lax) ; suppers Warning: Unused lexical argument `lax'
  (cond
   ((equal string "") "")
   (t  (concat firstly-search-regex string))))

(defun firstly-search--isearch-repeat-backward (&optional arg)
  "Supress `visible-bell' on error when reaching bob.
With a numeric argument, repeat the search ARG times.
Begining of the buffer. This error is hard to fix other way."
  (interactive "P")
  (let ((visible-bell nil))
    (isearch-repeat-backward arg)))

(defun firstly-search--isearch-repeat-forward (&optional arg)
  "Supress `visible-bell' on error when reaching eob.
With a numeric argument, repeat the search ARG times.
End of the buffer. This error is hard to fix other way."
  (interactive "P")
  (let ((visible-bell nil))
    (isearch-repeat-forward arg)))

;; create copy of isearch-mode-map. Activates after typing.
(defvar-keymap firstly-search-nav-map
      :parent isearch-mode-map
      "C-p" #'firstly-search--isearch-repeat-backward
      "C-n" #'firstly-search--isearch-repeat-forward
      ;; "C-m" #'dired-find-file
      )

(defun firstly-search--isearch-change-map ()
  "Speed up navigation by rebinding active isearch keys."
    ;; - fix that exit search and do other work
    ;; modify copy if `isearch-mode-map'
    (keymap-unset firstly-search-nav-map "C-m") ;; this do not modify original in fact
    ;; (keymap-unset firstly-search-nav-map "RET") ;; this do not modify original in fact
    (keymap-unset firstly-search-nav-map "<return>") ;; this do not modify original in fact
    ;; -- copy isearch map to create our replacement
    (if (not firstly-search--saved-isearch-mode-map) ; if was saved don't rewrite
        (setq firstly-search--saved-isearch-mode-map isearch-mode-map))
    (setq isearch-mode-map firstly-search-nav-map))


(defun firstly-search--isearch-mode-end-hook ()
  "Disable navigation."
  (when firstly-search--isearch-navigation-flag
    (setq firstly-search--isearch-navigation-flag nil) ;; called once
    ;; restore isearch options
    (setq isearch-wrap-pause firstly-search--saved-isearch-wrap-pause)
    (setq isearch-regexp-function firstly-search--saved-isearch-regexp-function)
    ;; attempt to clear our keymap modifications of isearch
    (setq isearch-mode-map firstly-search--saved-isearch-mode-map)
    ;; remove isearch advice
    (remove-function (local 'isearch-search-fun-function)
                     firstly-search--isearch-search-fun-function)
    (remove-hook 'isearch-mode-end-hook #'firstly-search--isearch-mode-end-hook t)))


(defun firstly-search--pre-command-hook ()
  "Advice to add alphabet fast navigation."
  (let* ((key (this-single-command-keys))
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

      ;; save and change isearch options
      (setq firstly-search--saved-isearch-wrap-pause isearch-wrap-pause)
      (setq isearch-wrap-pause 'no)

      ;; required for activation of isearch
      (isearch-forward nil t)
      ;; from begining of word or not
      (setq firstly-search--saved-isearch-regexp-function isearch-regexp-function)
      (setq isearch-regexp-function (if firstly-search-regex
                                        #'firstly-search--isearch-regexp-function
                                      #'word-search-regexp)) ; not from begining
      ;; activate isearch by file name
      (setq isearch-string (key-description key))
      (setq isearch-message (key-description key))
      (setq isearch-success t isearch-adjusted 'toggle)
      ;; replace current command
      (setq this-command #'isearch-repeat-forward) ; do nothing
      (add-hook 'isearch-mode-end-hook #'firstly-search--isearch-mode-end-hook nil t))
     ;; - clearn isearch for C-m, etc - keys not in isearch-mode-map
     ((and firstly-search--isearch-navigation-flag
           (not
           (or (memq this-command isearch-menu-bar-commands)
               (commandp (lookup-key isearch-mode-map key nil))
               (commandp
                (lookup-key
                 `(keymap (tool-bar menu-item nil ,isearch-tool-bar-map)) key)))))
      (setq isearch-string "")
      (setq isearch-message "")
      (isearch-done))
     ;; - speed up navigation
     ((and firstly-search--isearch-navigation-flag
           (eq last-command #'isearch-repeat-backward)
           (eq this-command 'isearch-repeat-forward))
      (call-interactively #'isearch-repeat-forward)) ;
     )))


(defun firstly-search--my-goto-match-beginning ()
  "Place cursor always at the end of search result.
Used for speed up navingation."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))


(defun firstly-search--check-same (str1 str2 str3)
  "Compare two string values both equal to 3 or not equal.
Argument STR1 string for comparision with STR3.
Argument STR2 string to comparision with STR3."
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
  (let ((pos1 pos)
        (prname (car property))
        (prvalue (cdr property)))
    ;; step to the next property
    (setq pos1 (next-single-property-change pos1 prname))
    ;; if current value is positive we just get next one
    ;; else look for positive
    (if (not (equal (get-text-property pos prname) prvalue))
        (while (and pos1 (not (equal (get-text-property pos1 prname) prvalue)))
          (setq pos1 (next-single-property-change pos1 prname))))
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
    (when pos1
      (setq prvalue-pos1 (get-text-property pos1 prname))
      (setq at-the-middle-flag (firstly-search--check-same prvalue-cur prvalue-pos1 prvalue))
      (if at-the-middle-flag
          (while (and pos2
                      (firstly-search--check-same (get-text-property pos2 prname) prvalue-cur prvalue))
            (setq pos1 pos2)
            (setq pos2 (previous-single-property-change pos1 prname)))
        ;; else at the edge
        (progn
          (setq pos2 pos1) ;; one step
          (while (and pos2
                      (not (firstly-search--check-same (get-text-property pos2 prname) prvalue-cur prvalue)))
            (setq pos1 pos2)
            (setq pos2 (previous-single-property-change pos1 prname)))))
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
  (apply-partially
   #'search-within-boundaries
   search-fun ; SEARCH-FUN
   (lambda (pos) ; GET-FUN - check if point is on property
     (let ((pos (if isearch-forward pos (max (1- pos) (point-min))))) ;; if backward pos = pos - 1
       (seq-some (lambda (property)
                   ;; equal to value. predicate for every property.
                   (equal (get-text-property pos (car property)) (cdr property) ))
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
                          (if pos
                              (firstly-search--previous-single-property-change-by-value property pos)))
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
