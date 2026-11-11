;;; firstly-search.el --- Search with any key: Dired, Package, Buffer menu modes  -*- lexical-binding: t -*-

;; Copyright (c) 2024-2026 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: matching, isearch, navigation, dired, packagemenu
;; URL: https://codeberg.org/Anoncheg/firstly-search
;; Created: 10 Sep 2024
;; Version: 0.2
;; Package-Requires: ((emacs "29.1") (compat "31.1"))

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

;; Any key activate navigation by tuned search.
;; There are minor modes for major modes: Dired, Package Menu.
;; Pressing any printable character moves the cursor, allowing
;; search-based navigation to major elements.

;;;; Activation:

;; - for Dired:	M-x firstly-search-dired-mode RET
;; - for Package Menu:	M-x firstly-search-package-mode RET
;; - for Buffer Menu:	M-x firstly-search-buffermenu-mode RET
;; - for Bookmarks:	M-x firstly-search-bookmarks-mode RET

;;;; Configuration (in .emacs):
;; (require 'firstly-search-dired)
;; (require 'firstly-search-package)
;; (require 'firstly-search-buffermenu)
;; (require 'firstly-search-bookmarks)
;; (add-hook 'dired-mode-hook #'firstly-search-dired-mode)
;; (add-hook 'package-menu-mode-hook #'firstly-search-package-mode)
;; (add-hook 'Buffer-menu-mode-hook #'firstly-search-buffermenu-mode)
;; (add-hook 'bookmark-bmenu-mode-hook #'firstly-search-bookmarks-mode)

;;;; Customization:

;; M-x customize-group RET firstly-search
;; M-x customize-group RET firstly-search-dired
;; M-x customize-group RET firstly-search-package
;; M-x customize-group RET firstly-search-buffermenu
;; M-x customize-group RET firstly-search-bookmarks

;;;; Note:

;; C-n and C-p is used during searching as C-s and C-r

;;;; How it works:

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

;; To reduce  speed we use `isearch-repeat-on-direction-change' and
;; `isearch-wrap-pause' variables.


;;;; Files:

;; - first-search.el - common functionality for other files.
;; - first-search-{dired,package,buffermenu,bookmarks,recentf}.el -
;;   define user available minor modes for specific major modes.

;;;; Other packages from author:

;; - Search with Chinese	https://github.com/Anoncheg1/pinyin-isearch
;; - Ediff no 3-th window	https://github.com/Anoncheg1/ediffnw
;; - Dired history		https://github.com/Anoncheg1/dired-hist
;; - Selected window contrast	https://github.com/Anoncheg1/selected-window-contrast
;; - Copy link to clipboard	https://github.com/Anoncheg1/emacs-org-links
;; - Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
;; - Restore buffer state	https://github.com/Anoncheg1/emacs-unmodified-buffer1
;; - outline.el usage		https://github.com/Anoncheg1/emacs-outline-it
;; - ai_block for Org mode for chat.  https://github.com/Anoncheg1/emacs-cui
;; - TAB key reimplementation	https://github.com/Anoncheg1/emacs-indent

;;;; Donate:

;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D

;;;; TODO:

;;; Code:

;; Touch: Let me see you stripped.

;; -=-= Variables
(declare-function word-search-regexp "isearch")

(defgroup firstly-search nil
  "Search with any key."
  :group 'firstly-search
  :prefix "firstly-search-")

(defcustom firstly-search-keys "abcdefghijklmnopqrstuvwxyz -_=+,." ; ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
  "Keys that activate navigation.
Those keys unset in `firstly-search--change-map' where major mode map
 replaced by our copy of t."
  :local t
  :type '(string)
  :group 'firstly-search)

(defcustom firstly-search-regex-prefix "\\_<"
  "Non-nil means add this as regex prefix to search string.
For empty string `word-search-regexp' is set to
 `isearch-regexp-function'.
This value used to search from begining of every word separated
by space.  You may use \\(\\_<\\|-\\) for search in words
separated by -."
  :type 'string
  :group 'firstly-search)


(defcustom firstly-search-isearch-prefix "filename "
  "Non-nil means add string to isearch prompt."
  :local t
  :type 'string
  :group 'firstly-search)

(defcustom firstly-search-renew-idle-seconds 3
  "Idle delay (seconds) before the renewal timer re-checks isearch state.
Small enough to feel immediate after a pause; large enough not to churn
while the user is actively typing.  To disable, set to nil."
  :type '(choice (const :tag "Disabled" nil)
                 number)
  :group 'firstly-search)

(defvar-local firstly-search--isearch-navigation-flag nil
  "Non-nil means firstly-search navigation activated.
Allow to separate firstly-search navigation from isearch.
May be sub-minor-mode.")

;; Snapshot the pristine isearch map at load time.  `isearch-mode-map'
;; is a global; nothing in this package may permanently replace it.
(defvar-local firstly-search--original-isearch-mode-map nil
  "Pristine copy of `isearch-mode-map' captured at load time.")

(defvar-local firstly-search--major-mode-saved-map nil
  "Saved major-mode-map.")

;; (defvar-local firstly-search-ignore-mode-map nil
;;   "Replacement for major-mode-map.")

(defvar-local firstly-search--isearch-search-fun-function nil
  "Used for advicing `isearch-search-fun-function' function.
This function should return funcation that call function
 `isearch-forward' with appropriate regex.")

;; -=-= saving isearch

(defvar-local firstly-search--isearch-saved-vars-stack nil
  "Alist of (SYMBOL . VALUE) saved by `firstly-search--do-isearch'.

Consumed by `firstly-search--isearch-mode-end-hook' to restore
isearch state that `firstly-search--do-isearch' mutates globally.")

(defun firstly-search--isearch-save-vars ()
  "Snapshot isearch variables mutated by `firstly-search--do-isearch'."
  (push (list (cons 'isearch-regexp-function           isearch-regexp-function)
              (cons 'isearch-wrap-pause                 isearch-wrap-pause)
              (cons 'isearch-repeat-on-direction-change isearch-repeat-on-direction-change)
              (cons 'isearch-message isearch-message)
              (cons 'isearch-string isearch-string))
        firstly-search--isearch-saved-vars-stack))

(defun firstly-search--isearch-restore-vars ()
  "Restore isearch variables saved by `firstly-search--isearch-save-vars'."
  (let ((saved (pop firstly-search--isearch-saved-vars-stack)))
    (when saved
      (dolist (pair saved)
        (set (car pair) (cdr pair))))))

;; -=-= Timer

(defvar firstly-search--renew-timer nil
  "Idle timer renewing while firstly-search navigation is active.")

(defun firstly-search--renew-timer-callback (&optional by-time)
  "Re-arm while isearch runs; on termination cancel and call the abort.
Depending on `isearch-success' renew or stop timer.
Optional argument BY-TIME used to detect call from timer, to indicate
 expiration of time."
  (when firstly-search-renew-idle-seconds ; if delete
    (when (timerp firstly-search--renew-timer)
      (cancel-timer firstly-search--renew-timer))

    (if (and isearch-mode isearch-success (not by-time))
        (setq firstly-search--renew-timer
              (run-with-idle-timer firstly-search-renew-idle-seconds
                                   nil #'firstly-search--renew-timer-callback t))
      ;; else: by-time - isearch-success is nil
      (setq firstly-search--renew-timer nil)
      (when firstly-search--isearch-navigation-flag
        (firstly-search--abort)))))

;; -=-= Functions

(defun firstly-search--isearch-regexp-function (string &optional _lax)
  "Build a regexp that anchors the user's input at a word start.
In literal mode, quote STRING so metacharacters match themselves.

In regexp mode, use STRING as the user's regexp body — do not re-quote —
 but still prepend `firstly-search-regex-prefix' so the match starts at
 the same anchor.

Optional argument LAX is accepted for compatibility with
`isearch-regexp-function' and ignored."
  (cond
   ((string-empty-p string) "")
   (isearch-regexp
    ;; user is typing a regexp; honour it verbatim after the anchor
    (concat firstly-search-regex-prefix string))
   (t
    ;; literal search; protect metacharacters in filenames etc.
    (concat firstly-search-regex-prefix (regexp-quote string)))))

(defun firstly-search--isearch-repeat-backward (&optional arg)
  "Supress `visible-bell' on error when reaching bob.
With a numeric argument, repeat the search ARG times.
Begining of the buffer.  This error is hard to fix other way."
  (interactive "P")
  (let ((visible-bell nil))
    (isearch-repeat-backward arg)))

(defun firstly-search--isearch-repeat-forward (&optional arg)
  "Supress `visible-bell' on error when reaching eob.
With a numeric argument, repeat the search ARG times.
End of the buffer.  This error is hard to fix other way."
  (interactive "P")
  (let ((visible-bell nil))
    (isearch-repeat-forward arg)))

;; -=-= keys Map

(defun firstly-search--isearch-change-map ()
  "Install `firstly-search-nav-map' as `isearch-mode-map' for this session.
The original map is kept in `firstly-search--original-isearch-mode-map'
and restored unconditionally by `firstly-search--isearch-mode-end-hook'."
  ;; Always start from a clean copy so consecutive activations do not
  ;; accumulate stale rebindings, and so a previously-aborted session
  ;; cannot leak a swapped map into this one.
  (unless firstly-search--original-isearch-mode-map
    (setq-local firstly-search--original-isearch-mode-map (copy-keymap isearch-mode-map)))
  ;; replace isearch map
  (let ((map (define-keymap :parent firstly-search--original-isearch-mode-map)))
    (keymap-set map "C-p" #'firstly-search--isearch-repeat-backward)
    (keymap-set map "C-n" #'firstly-search--isearch-repeat-forward)
    (keymap-unset map "C-m")
    (keymap-unset map "<return>") ; working?
    (setq isearch-mode-map map)))

(defun firstly-search--change-map (map)
  "Save the current major mode's keymap, then install MAP as local map.
If MAP is non-nil, unbind
all keys in `firstly-search-keys' from MAP and replace major mode with it.
If MAP is nil, only the save happens."
  (unless firstly-search--major-mode-saved-map ; save
    (setq-local firstly-search--major-mode-saved-map
                (copy-keymap (symbol-value (intern (concat (symbol-name major-mode) "-map"))))))
  (when map
    (dolist (c (string-to-list firstly-search-keys))
      (unless (= c 32) ; not space
        (define-key map (vector c) nil)))
    (use-local-map map)))


(defun firstly-search--restore-map ()
  "Restore major mode keys, saved at activation of firstly-search."
  (use-local-map firstly-search--major-mode-saved-map))

(defun firstly-search--isearch-mode-end-hook ()
  "Disable navigation and restore global isearch state.
Called by `isearch-done' function."
  (when firstly-search--isearch-navigation-flag
    (firstly-search--isearch-restore-vars)

    ;; restore global isearch options from the pristine snapshot
    (setq isearch-mode-map firstly-search--original-isearch-mode-map)

    ;; drop our advice (idempotent)
    (when (advice-member-p firstly-search--isearch-search-fun-function
                           'isearch-search-fun-function)
      (remove-function (local 'isearch-search-fun-function)
                       firstly-search--isearch-search-fun-function))

    (remove-hook 'isearch-mode-end-hook
                 #'firstly-search--isearch-mode-end-hook t)

    (advice-remove 'isearch-repeat #'firstly-search--abort-on-failed-repeat)

    (setq firstly-search--isearch-navigation-flag nil)))

;; -=-= hooks
(defun firstly-search--my-goto-match-beginning-hook ()
  "Place cursor always at the end of search result.
Used for speed up navingation."
  (when (and (not isearch-forward)
             isearch-other-end
             firstly-search--isearch-navigation-flag)
    (goto-char isearch-other-end)))


(defun firstly-search--do-isearch (str)
  "Also used for testing with STR."
  ;; 0. snapshot the isearch vars we are about to clobber
  (firstly-search--isearch-save-vars)


  ;; 1. install advice + keymap
  (when firstly-search--isearch-search-fun-function
    (add-function :around (local 'isearch-search-fun-function)
                  firstly-search--isearch-search-fun-function
                  ;; add "filename "
                  (list (cons 'isearch-message-prefix firstly-search-isearch-prefix))))

  (advice-add 'isearch-repeat :after #'firstly-search--abort-on-failed-repeat)
  (firstly-search--isearch-change-map)

  ;; 2. mark navigation active *before* entering isearch
  (setq firstly-search--isearch-navigation-flag t)
  (add-hook 'isearch-mode-end-hook #'firstly-search--isearch-mode-end-hook nil t)

  ;; 4. enter isearch (may run for a long time)
  (isearch-forward nil t)
  ;; 3. install our options (works only after run search)
  (setq isearch-regexp-function (if (stringp firstly-search-regex-prefix)
                                    #'firstly-search--isearch-regexp-function
                                  ;; else for ""
                                  (when (string-empty-p firstly-search-regex-prefix)
                                    #'word-search-regexp)
                                  ;; else = nil
                                  ))
  (setq isearch-wrap-pause 'no-ding)
  (setq isearch-repeat-on-direction-change t)

  ;; 5. seed with the triggering key - inside the let so the search loop sees our bindings
  (setq isearch-string str
        isearch-message str
        isearch-success t)
  ;; replace current command
  (setq this-command #'isearch-repeat-forward))


(defun firstly-search--pre-command-hook ()
  "Activate Firstly Search.
Added locally to buffers by mirod Firstly Search modes.

If search mode is not active and key in `firstly-search-keys' we
 activate navigation."
  (unless (or isearch-mode
              firstly-search--isearch-navigation-flag)
    (let* ((key (this-single-command-keys))
           (key-des (key-description key)))
      (when (and (integerp (aref key 0))
                 (not (= (aref key 0) 32)) ; not space
                 (memq (aref key 0) (string-to-list firstly-search-keys)))
        (firstly-search--do-isearch key-des)))))


(defun firstly-search--abort ()
  "Full isearch forced stop."
  (isearch-done) ; call `isearch-mode-end-hook' that call `firstly-search--isearch-mode-end-hook' that restire isearch options.
  ;; `isearch-cancel` without moving cursor back.
  (isearch-clean-overlays))


(defun firstly-search--abort-on-failed-repeat (&rest _)
  "Do at failed search cancel search completely.
Used in `isearch-repeat', stop isearch on failure."
  (when (and isearch-mode
             firstly-search--isearch-navigation-flag)
    (firstly-search--renew-timer-callback)
    (when (not isearch-success)
      (firstly-search--abort)
      (signal 'quit nil))))

;; -=-= Functions

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
  "Return the start of the previous run of PROPERTY whose value matches.
PROPERTY is (NAME . VALUE).  POS is the starting position.

Symmetric with `firstly-search--next-single-property-change-by-value'.
Returns nil when no earlier matching run exists."
  (let* ((prname  (car property))
         (prvalue (cdr property))
         (prev    (previous-single-property-change pos prname)))
    (cond
     ;; POS is inside a matching run: the boundary just before it
     ;; already satisfies the caller's contract.
     ((equal (get-text-property pos prname) prvalue) prev)
     ;; POS is inside a non-matching run: PREV is its start; the run
     ;; *before* PREV is the one we want, and its value is read at
     ;; (1- PREV).
     ((and prev (> prev (point-min))
           (equal (get-text-property (1- prev) prname) prvalue))
      prev)
     ;; No adjacent match: keep stepping back until we find one.
     ;; (Kept only for the rare “gap” case where the adjacent run is
     ;; a different non-matching value; in practice this is one step.)
     (t
      (while (and prev
                  (not (equal (get-text-property (1- prev) prname) prvalue))
                  (> prev (point-min)))
        (setq prev (previous-single-property-change prev prname)))
      prev))))


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
