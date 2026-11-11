;;; firstly-search-ibuffer.el --- Basename isearch for `ibuffer-mode'  -*- lexical-binding: t -*-

;; Copyright (c) 2024-2026 github.com/Anoncheg1
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

;; Ibuffer doesn't make it easy to determine column positions: there
;;  are no text properties for columns, and text positions are
;;  recalculated according to the "format" to fit a resizing window.

;;;; Activation:
;;   (require 'firstly-search-ibuffer)
;;   (with-eval-after-load 'ibuffer
;;     (add-hook 'ibuffer-mode-hook #'firstly-search-ibuffer-mode))

;;; Code:
(require 'firstly-search)
(require 'ibuffer)

(defcustom firstly-search-ibuffer-isearch-prefix " "
  "Hint showed in isearch."
  :type 'string
  :group 'firstly-search-ibuffer)

(defcustom firstly-search-ibuffer-string-prefix "\\(\\_<\\|*\\)" ; "\\b"
  "Add this as regex prefix to search string."
  :type 'string
  :group 'firstly-search-package)


(defvar-keymap firstly-search-ibuffer-map-replacement
  :doc "Rebinding of keys that interfere with firstly search.
Replacement for `ibuffer-mode-map'.
Optional, may be nil.
It have bindings for commands that unset `firstly-search--change-map'.
When it have name `{major-mode}-map` it is visible and applied in
 parallel, that is why we use other name."
  :parent ibuffer-mode-map
  "M-s a"	#'ibuffer-do-sort-by-alphabetic
  "M-s f"	#'ibuffer-do-sort-by-filename/process
  "M-s i"	#'ibuffer-invert-sorting
  "M-s m"	#'ibuffer-do-sort-by-major-mode
  "M-s s"	#'ibuffer-do-sort-by-size
  "M-s v"	#'ibuffer-do-sort-by-recency

  ;; "M-`"	#'ibuffer-switch-format
  "M-b"	#'ibuffer-bury-buffer
  "M-d"	#'ibuffer-mark-for-delete
  "M-e"	#'ibuffer-visit-buffer
  "M-f"	#'ibuffer-visit-buffer
  ;; "M-e .. f"	#'ibuffer-visit-buffer
  "M-g"	#'ibuffer-update
  "M-h"	#'describe-mode
  "M-j"	#'ibuffer-jump-to-buffer
  "M-k"	#'ibuffer-do-kill-lines
  "M-l"	#'ibuffer-redisplay
  "M-m"	#'ibuffer-mark-forward
  "M-n"	#'ibuffer-forward-line
  "M-o"	#'ibuffer-visit-buffer-other-window
  "M-p"	#'ibuffer-backward-line
  "M-q"	#'quit-window
  "M-r"	#'ibuffer-do-replace-regexp
  "M-t"	#'ibuffer-toggle-marks
  "M-u"	#'ibuffer-unmark-forward
  "C-M-v"	#'ibuffer-do-view
  "C-M-w"	#'ibuffer-copy-filename-as-kill
  "C-M-x"	#'ibuffer-do-kill-on-deletion-marks
  ;; "M-|"	#'ibuffer-do-shell-command-pipe
  ;; "M-~"	#'ibuffer-do-toggle-modified
  )

(defun firstly-search-ibuffer--filter-header (beg end)
  "Return non-nil if text between BEG and END is not header and footer.
This function is intended to be used as `isearch-filter-predicate'."
  (let ((after-header (save-excursion
                        (goto-char (point-min))
                        (forward-line 3)
                        (point)))
        (footer (save-excursion
                  (goto-char (point-max))
                  (line-beginning-position))))
    (and (not (text-property-not-all beg end 'inhibit-isearch nil))
         (or (eq search-invisible t)
             (not (isearch-range-invisible beg end)))

         (>= beg after-header)
         (<= end footer))))

;;;###autoload
(define-minor-mode firstly-search-ibuffer-mode
  "Instant basename search for `ibuffer-mode' buffers."
  :lighter " Fsearch"
  :global nil
  :group 'firstly-search
  (if firstly-search-ibuffer-mode
      (progn
        (setq-local firstly-search-isearch-prefix firstly-search-ibuffer-isearch-prefix)
        (setq-local firstly-search-regex-prefix firstly-search-ibuffer-string-prefix)
        (firstly-search--change-map firstly-search-ibuffer-map-replacement)
        (add-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning-hook nil t) ; for speed
        (add-hook 'pre-command-hook #'firstly-search--pre-command-hook nil t)
        (setq-local isearch-filter-predicate #'firstly-search-ibuffer--filter-header)) ; skip header

    (remove-hook 'pre-command-hook #'firstly-search--pre-command-hook t)
    (remove-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning-hook t)
    (firstly-search--restore-map)
    (kill-local-variable 'isearch-filter-predicate)) ; skip header
  )

(provide 'firstly-search-ibuffer)
;;; firstly-search-ibuffer.el ends here
