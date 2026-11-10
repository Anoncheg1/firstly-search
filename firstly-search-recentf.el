;;; firstly-search-recentf.el --- Basename isearch for `recentf-dialog-mode'  -*- lexical-binding: t -*-

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
;
;; Activates `firstly-search' in the `*Open Recent*' buffer produced by
;; `recentf-open-files': typing any printable character that is not
;; already bound to a command in `recentf-dialog-mode-map' starts an
;; incremental search restricted to the *basename* of each recent file.

;; Unlike the Dired and Bookmark integrations, search by properties
;; `isearch-search-fun-in-text-property' and
;; `firstly-search-fun-match-text-property' cannot be used here.
;; We search by boundaries with
;; `isearch-search-fun-in-noncontiguous-region' and
;; `firstly-search-recentf--bounded-isearch' here.
;; We place boundaris at part of paths with basename to search on them
;; only.
;;
;; Personally, I load recentf with commands:
;; (recentf-load-list)
;; (recentf-open-files)

;;;; Activation:
;;   (with-eval-after-load 'recentf
;;     (add-hook 'recentf-dialog-mode-hook #'firstly-search-recentf-mode))

;;; Code:
(require 'firstly-search)
(require 'recentf)

(defgroup firstly-search-recentf nil
  "For major mode `recentf-mode'."
  :group 'firstly-search-recentf)

(defcustom firstly-search-recentf-isearch-prefix "Basename "
  "Hint showed in isearch."
  :type 'string
  :group 'firstly-search-recentf)

(defcustom firstly-search-recentf-basename-flag t
  "Non-nil means search will limited to basenames of paths.
Used in `isearch-regexp-function'."
  :type 'boolean
  :group 'firstly-search-recentf)

(defcustom firstly-search-recentf-from-start-flag t
  "Non-nil means search will be from begining after path separator.
Works only if `firstly-search-recentf-basename-flag` is true.
Used in `isearch-regexp-function'."
  :type 'boolean
  :group 'firstly-search-recentf)

(defvar firstly-search-recentf--path-separators
  (if (memq system-type '(ms-dos windows-nt cygwin)) "\\" "/")
  "Characters that may separate directories in a recentf path.
Used in `isearch-regexp-function'.")

(defcustom firstly-search-recentf-regex-prefix (concat "\\(?:[_\."
                                                       firstly-search-recentf--path-separators
                                                       "-]\\)")
  "Regex for word search in package names."
  :type 'string
  :group 'firstly-search-recentf)

(defun firstly-search-recentf--basename ()
  "Return (BASENAME . OFFSET) for the path on the current line, or nil.
OFFSET is the 0-based index of BASENAME within the line's raw text."
  (let* ((raw  (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (path (string-trim
                (replace-regexp-in-string
                 "\\`\\[[0-9]+\\][ \t]+" "" (string-trim raw)))))
    (when (and (not (string-empty-p path))
               (or (file-name-absolute-p path)
                   (string-prefix-p "~" path)))
      (let* ((base     (file-name-nondirectory
                        (directory-file-name (expand-file-name path))))
             (path-off (string-match (regexp-quote path) raw))
             (trimmed  (directory-file-name path))
             (base-rel (- (length trimmed) (length base))))
        (cons base (+ path-off base-rel))))))


(defun firstly-search-recentf--bounds-with-slash ()
  "Return (START . END) ranges of basenames on path lines of *recentf*.
Suitable as BOUNDS for `isearch-search-fun-in-noncontiguous-region'.

We add first / character before basename boundary to allow search from
 begin of basename with help of `firstly-search-regex-prefix'."
  (save-excursion
    (goto-char (point-min))
    (let (bounds)
      (while (not (eobp))
        (let ((hit (firstly-search-recentf--basename))
              (beg (line-beginning-position)))
          (when hit
            (push (cons (+ beg (1- (cdr hit)))
                        (+ beg (1- (cdr hit)) (1+ (length (car hit)))))
                  bounds)))
        (forward-line 1))
      (nreverse bounds))))


(defun firstly-search-recentf--bounds-normal ()
  "Return (START . END) ranges of basenames on path lines of *recentf*.
Suitable as BOUNDS for `isearch-search-fun-in-noncontiguous-region'."
  (save-excursion
    (goto-char (point-min))
    (let (bounds)
      (while (not (eobp))
        (let ((hit (firstly-search-recentf--basename))
              (beg (line-beginning-position)))
          (when hit
            (push (cons (+ beg (cdr hit))
                        (+ beg (cdr hit) (length (car hit))))
                  bounds)))
        (forward-line 1))
      (nreverse bounds))))

(defun firstly-search-recentf--bounds-full ()
  "Return (START . END) ranges with just one for whole recentf buffer.
Suitable as BOUNDS for `isearch-search-fun-in-noncontiguous-region'."
  (save-excursion
    (goto-char (point-min))
    (forward-line 3)
    (list (cons (point)
                (progn (goto-char (point-max))
                       (forward-line -1)
                       (point))))))


(defun firstly-search-recentf--bounded-isearch (orig-fun)
  "Return the function that searches inside basenames in recentf buffer.
The returned function narrows the search in basenames bounds in form of
 list of (START . END).
Replacement for `isearch-search-fun-function'.
Argument ORIG-FUN isearch internal function."
  (let ((bounds (if firstly-search-recentf-basename-flag
                    (if firstly-search-recentf-from-start-flag
                        (firstly-search-recentf--bounds-with-slash)
                      ;; else
                      (firstly-search-recentf--bounds-normal))
                  ;; else
                  (firstly-search-recentf--bounds-full))))
    (isearch-search-fun-in-noncontiguous-region (funcall orig-fun) bounds)))

(defvar-keymap firstly-search-recentf-map
  :doc "Rebinding of keys that interfere with firstly search.
Replacement for `recentf-dialog-mode-map'.
Optional, may be nil.
It have bindings for commands that unset `firstly-search--change-map'.
When it have name `{major-mode}-map` it is visible and applied in
 parallel, that is why we use other name."
  :parent recentf-dialog-mode-map
  "M-q"	#'recentf-cancel-dialog
  "M-0"	#'recentf-open-most-recent-file-0
  "M-1"	#'recentf-open-most-recent-file-1
  "M-2"	#'recentf-open-most-recent-file-2
  "M-3"	#'recentf-open-most-recent-file-3
  "M-4"	#'recentf-open-most-recent-file-4
  "M-5"	#'recentf-open-most-recent-file-5
  "M-6"	#'recentf-open-most-recent-file-6
  "M-7"	#'recentf-open-most-recent-file-7
  "M-8"	#'recentf-open-most-recent-file-8
  "M-9"	#'recentf-open-most-recent-file-9)

;;;###autoload
(define-minor-mode firstly-search-recentf-mode
  "Instant basename search for `recentf-dialog-mode' buffers."
  :lighter " Fsearch"
  :global nil
  :group 'firstly-search
  (if firstly-search-recentf-mode
      (progn
        (setq-local firstly-search-isearch-prefix firstly-search-recentf-isearch-prefix)
        (setq-local firstly-search-regex-prefix firstly-search-recentf-regex-prefix)
        (setq-local firstly-search-regex-prefix (when firstly-search-recentf-from-start-flag
                                                   firstly-search-recentf-regex-prefix))
        (setq-local firstly-search--isearch-search-fun-function
                    #'firstly-search-recentf--bounded-isearch)
        (firstly-search--change-map firstly-search-recentf-map)
        (add-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning-hook nil t) ; for speed
        (add-hook 'pre-command-hook #'firstly-search--pre-command-hook nil t))
    (remove-hook 'pre-command-hook #'firstly-search--pre-command-hook t)
    (remove-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning-hook t)
    (firstly-search--restore-map)))

(provide 'firstly-search-recentf)
;;; firstly-search-recentf.el ends here
