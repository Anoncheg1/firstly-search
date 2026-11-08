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
Used in `isearch-regexp-function'."
  :type 'boolean
  :group 'firstly-search-recentf)

(defvar firstly-search-recentf--path-separators
  (if (memq system-type '(ms-dos windows-nt cygwin)) "\\" "/")
  "Characters that may separate directories in a recentf path.
Used in `isearch-regexp-function'.")


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

(defvar firstly-search-recentf-ignore-map
  (let ((map (copy-keymap recentf-dialog-mode-map)))
    (define-key map (kbd "n") nil)
    (define-key map (kbd "p") nil)
    map)
  "Copy of `recentf-dialog-mode-map' without the n, p, q keys.

Used by `firstly-search-recentf-mode' as `firstly-search-ignore-mode-map'
so that those keys fall through to the search machinery instead of being
handled by the dialog keymap.")


;;;###autoload
(define-minor-mode firstly-search-recentf-mode
  "Instant basename search for `recentf-dialog-mode' buffers."
  :lighter " Fsearch"
  :global nil
  :group 'firstly-search
  (if firstly-search-recentf-mode
      (progn
        (setq-local firstly-search-isearch-prefix firstly-search-recentf-isearch-prefix)
        (setq-local firstly-search-regex-prefix (when firstly-search-recentf-from-start-flag
                                                   firstly-search-recentf--path-separators)) ; nil by default
        (setq-local firstly-search-ignore-mode-map firstly-search-recentf-ignore-map)
        (setq-local firstly-search--isearch-search-fun-function
                    #'firstly-search-recentf--bounded-isearch)
        (add-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning-hook nil t) ; for speed
        (add-hook 'pre-command-hook #'firstly-search--pre-command-hook nil t))
    (remove-hook 'pre-command-hook #'firstly-search--pre-command-hook t)
    (remove-hook 'isearch-update-post-hook #'firstly-search--my-goto-match-beginning-hook t)))

(provide 'firstly-search-recentf)
;;; firstly-search-recentf.el ends here
