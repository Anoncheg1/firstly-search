;;; firstly-search-anymode.el --- Two functions: activate and deactivate for any mode  -*- lexical-binding: t -*-

;; Copyright (c) 2024-2026 github.com/Anoncheg1,codeberg.org/Anoncheg
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
;; To activate:
;; (require 'firstly-search-anymode)
;; M-x firstly-search-anymode-ibuffer-activate

(require 'firstly-search)

;;; Code:

(defvar-local firstly-search-anymode--activated-params nil
  "Plist of parameters used by `firstly-search-anymode-activate'.
Non-nil means firstly-search is active in this buffer.  The plist stores
only the keys that were actually supplied, so `firstly-search-anymode-deactivate'
can undo exactly what was applied.")

;; -=-= Two functions

;;;###autoload
(defun firstly-search-anymode-activate (&rest args)
  "Universally activate firstly-search in the current buffer.
ARGS is a plist; required logic is always applied, optional keys only
when supplied.  Saves the effective parameters in
`firstly-search-anymode--activated-params' so
`firstly-search-anymode-deactivate' can undo them precisely.

Recognized keys:
  :isearch-prefix    STRING   -> `firstly-search-isearch-prefix'
  :regex-prefix      STRING   -> `firstly-search-regex-prefix'
  :map               KEYMAP   -> passed to `firstly-search--change-map'
  :filter-predicate  FUNCTION -> `isearch-filter-predicate'

Returns the saved parameter plist."
  (let* ((isearch-prefix   (plist-get args :isearch-prefix))
         (regex-prefix     (plist-get args :regex-prefix))
         (map              (plist-get args :map))
         (filter-predicate (plist-get args :filter-predicate))
         (applied '()))
    ;; --- optional locals, recording only what we actually set ---
    (when isearch-prefix
      (setq-local firstly-search-isearch-prefix isearch-prefix)
      (setq applied (plist-put applied :isearch-prefix isearch-prefix)))
    (when regex-prefix
      (setq-local firstly-search-regex-prefix regex-prefix)
      (setq applied (plist-put applied :regex-prefix regex-prefix)))

    (firstly-search--change-map map)
    (when map
      (setq applied (plist-put applied :map map)))

    (when filter-predicate
      (setq-local isearch-filter-predicate filter-predicate)
      (setq applied (plist-put applied :filter-predicate filter-predicate)))
    ;; --- required hooks (always) ---
    (add-hook 'isearch-update-post-hook
              #'firstly-search--my-goto-match-beginning-hook nil t)
    (add-hook 'pre-command-hook
              #'firstly-search--pre-command-hook nil t)
    ;; --- save state ---
    (setq firstly-search-anymode--activated-params applied)
    applied))

;;;###autoload
(defun firstly-search-anymode-deactivate ()
  "Deactivate firstly-search in the current buffer.
Undoes exactly the parameters saved by `firstly-search-anymode-activate'
 in `firstly-search-anymode--activated-params', plus the always-applied
 hooks.
Safe to call when not active (it is a no-op)."
  (interactive)
  (when firstly-search-anymode--activated-params
    (let ((applied firstly-search-anymode--activated-params))
      ;; --- optional locals ---
      (when (plist-get applied :isearch-prefix)
        (kill-local-variable 'firstly-search-isearch-prefix))
      (when (plist-get applied :regex-prefix)
        (kill-local-variable 'firstly-search-regex-prefix))
      (when (plist-get applied :filter-predicate)
        (kill-local-variable 'isearch-filter-predicate))
      (when (plist-get applied :map)
        (firstly-search--restore-map))
      ;; --- required hooks (always) ---
      (remove-hook 'pre-command-hook
                   #'firstly-search--pre-command-hook t)
      (remove-hook 'isearch-update-post-hook
                   #'firstly-search--my-goto-match-beginning-hook t)
      ;; --- clear state ---
      (setq firstly-search-anymode--activated-params nil))))

;; -=-= ibuffer presets (fully automatic) and the drop-in minor mode

(require 'firstly-search-ibuffer)

(defun firstly-search-anymode-ibuffer-activate ()
  "Activate firstly-search in the current ibuffer buffer, fully automatic."
  (interactive)
  (firstly-search-anymode-activate
   :isearch-prefix   firstly-search-ibuffer-isearch-prefix
   :regex-prefix     firstly-search-ibuffer-string-prefix
   :map              firstly-search-ibuffer-map
   :filter-predicate #'firstly-search-ibuffer--filter-header))

;;;###autoload
(defun firstly-search-anymode-ibuffer-deactivate ()
  "Deactivate firstly-search in the current ibuffer buffer."
  (interactive)
  (firstly-search-anymode-deactivate))

;;;###autoload
(define-minor-mode firstly-search-anymode-ibuffer-mode
  "Instant basename search for `ibuffer-mode' buffers."
  :lighter " Fsearch"
  :global nil
  :group 'firstly-search-anymode
  (if firstly-search-anymode-ibuffer-mode
      (firstly-search-anymode-ibuffer-activate)
    (firstly-search-anymode-ibuffer-deactivate)))

(provide 'firstly-search-anymode)

;;; firstly-search-anymode.el ends here
