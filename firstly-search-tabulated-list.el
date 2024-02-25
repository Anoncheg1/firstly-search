;;; firstly-search-tabulated-list.el --- Fast navigation for Package Menu mode -*- lexical-binding: t -*-

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
;; (TODO)
;; Minor mode for `tabulated-list-mode'.
;;
;; to activate, add lines to your Emacs configuration:
;; (require 'firstly-search-tabulated-list)
;; (add-hook 'tabulated-list-mode-hook #'firstly-search-tabulated-list-mode) ; todo

;;; Code:

(defvar-keymap firstly-search-tabulated-list-mode-map
  :doc "Replacement for `tabulated-list-mode-map'."
  "M-?"	#'describe-mode
  "M-h"	#'describe-mode
  ;; ">" #'end-of-buffer
  ;; "<" #'beginning-of-buffer
  "M-g"	#'revert-buffer
  ;; `tabulated-list-mode-map':
  "C-n"		#'next-line
  "C-p"		#'previous-line
  "M-<left>"		#'tabulated-list-previous-column
  "M-<right>"		#'tabulated-list-next-column
  "M-S"		#'tabulated-list-sort
  "M-}"		#'tabulated-list-widen-current-column
  "M-{"		#'tabulated-list-narrow-current-column
  "<follow-link>"	'mouse-face
  "<mouse-2>"		#'mouse-select-window)


(provide 'firstly-search-tabulated-list)
;;; firstly-search-tabulated-list.el ends here
