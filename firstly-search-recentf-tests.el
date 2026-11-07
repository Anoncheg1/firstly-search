;;; firstly-search-recentf-tests.el --- Tests -*- lexical-binding: t -*-

;; emacs -Q --batch --no-site-file -l ert.el -l firstly-search-recentf-tests.el -f ert-run-tests-batch-and-exit || exit 1

;;; Code:
(require 'ert)
(require 'firstly-search-recentf)

(defmacro fsr--with-buffer (paths &rest body)
  "Run BODY in a fresh recentf dialog built from PATHS."
  (declare (indent 1))
  `(let ((recentf-list (mapcar #'expand-file-name (list ,@paths))))
     (recentf-open-files)
     (let ((buf (get-buffer (format "*%s*" recentf-menu-title))))
       (unwind-protect
           (with-current-buffer buf
             (firstly-search-recentf-mode 1)
             ,@body)
         (firstly-search-recentf-mode -1)
         (when (buffer-live-p buf) (kill-buffer buf))))))

;; -=-= old

(defun my/recentf-isearch-search-fun ()
  (if (derived-mode-p 'recentf-dialog-mode)
      (lambda (string bound noerror)
        (let ((case-fold-search isearch-case-fold-search)
              (found nil)
              (regexp (regexp-quote string)))
          (save-excursion
            (while (and (not found)
                        (if isearch-forward
                            (search-forward "\n" bound t)
                          (search-backward "\n" bound t)))
              (let* ((bol  (line-beginning-position))
                     (line (buffer-substring-no-properties
                            bol (line-end-position)))
                     (file (if (string-match "\\`\\[[0-9]\\] \\(.*\\)\\'" line)
                               (match-string 1 line)
                             line))
                     (base (and (file-name-absolute-p file)
                                (file-name-nondirectory file))))
                (when (and base (string-match-p regexp base))
                  (setq found t)
                  (goto-char bol))))))
          (or found
              (unless noerror
                (signal 'search-failed (list string))))))
    (isearch-search-fun-default))

(defun my/recentf-dialog-mode-setup ()
  "Install the basename Isearch search function in the dialog buffer."
  (setq-local isearch-search-fun-function
              #'my/recentf-isearch-search-fun))

;; (add-hook 'recentf-dialog-mode-hook #'my/recentf-dialog-mode-setup)

;; -=-= For `firstly-search-recentf--isearch-search-fun'


(defun fsr--line-of (path)
  "Return BOL of the link line whose path is PATH, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((target (expand-file-name path))
          found)
      (while (and (not found) (not (eobp)))
        (when (equal target
                     (ignore-errors
                       (firstly-search-recentf--line-path)))
          (setq found (line-beginning-position)))
        (forward-line 1))
      found)))


(ert-deftest fsr-basename-from-start-t-t ()
    (let ( ; (isearch-forward t)
          (isearch-case-fold-search nil)
          (firstly-search-recentf-basename-flag t)
          (firstly-search-recentf-from-start-flag t))


      (fsr--with-buffer ("/tmp/only.el")
        (save-window-excursion
          (goto-char (point-min))
          (should (= 1 (progn (firstly-search--do-isearch "Click")
                              ;; (isearch-repeat-forward)
                              (point) ; return 1 on failure
                              )))
          (isearch-done)

          (goto-char (point-min))
          (should (= 1 (progn (firstly-search--do-isearch "y.el")
                              ;; (isearch-repeat-forward)
                              (point) ; return 1 on failure
                              )))
          (isearch-done)


        (goto-char (point-min))
        (should (= 120 (progn (firstly-search--do-isearch "only")
                              (isearch-repeat-forward)
                              (point) ; return 1 on failure
                              )))
        (isearch-done)

        (goto-char (point-min))
        (should (= 1 (progn (firstly-search--do-isearch "tmp")
                              ;; (isearch-repeat-forward)
                              (point) ; return 1 on failure
                              )))
        (isearch-done)))))

(ert-deftest fsr-basename-from-start-t-nil ()
    (let ( ; (isearch-forward t)
          (isearch-case-fold-search nil)
          (firstly-search-recentf-basename-flag t)
          (firstly-search-recentf-from-start-flag nil))


      (fsr--with-buffer ("/tmp/only.el")
        (save-window-excursion
          (goto-char (point-min))
          (should (= 1 (progn (firstly-search--do-isearch "Click")
                              ;; (isearch-repeat-forward)
                              (point)))) ; return 1 on failure
          (isearch-done)

          (goto-char (point-min))
          (should (= 123 (progn (firstly-search--do-isearch "y.el")
                                (isearch-repeat-forward)
                                (point)))) ; return 1 on failure
          (isearch-done)

          (goto-char (point-min))
          (should (= 120 (progn (firstly-search--do-isearch "only")
                                (isearch-repeat-forward)
                                (point)))) ; return 1 on failure
          (isearch-done)

          (goto-char (point-min))
          (should (= 1 (progn (firstly-search--do-isearch "tmp")
                              ;; (isearch-repeat-forward)
                              (point)))) ; return 1 on failure
          (isearch-done)))))

(ert-deftest fsr-basename-from-start-nil-t ()
    (let ( ; (isearch-forward t)
          (isearch-case-fold-search nil)
          (firstly-search-recentf-basename-flag nil)
          (firstly-search-recentf-from-start-flag t))

      (fsr--with-buffer ("/tmp/only.el")
        (save-window-excursion
          (goto-char (point-min))
          (should (= 1 (progn (firstly-search--do-isearch "Click")
                              ;; (isearch-repeat-forward)
                              (point)))) ; return 1 on failure
          (isearch-done)

          (goto-char (point-min))
          (should (= 1 (progn (firstly-search--do-isearch "y.el")
                              ;; (isearch-repeat-forward)
                              (point)))) ; return 1 on failure
          (isearch-done)

          (goto-char (point-min))
          (should (= 120 (progn (firstly-search--do-isearch "only")
                                (isearch-repeat-forward)
                                (point)))) ; return 1 on failure
          (isearch-done)

          (goto-char (point-min))
          (should (= 115 (progn (firstly-search--do-isearch "tmp")
                                (isearch-repeat-forward)
                                (point)))) ; return 1 on failure
          (isearch-done)))))

(ert-deftest fsr-basename-from-start-nil-nil ()
  (let ( ; (isearch-forward t)
        (isearch-case-fold-search nil)
        (firstly-search-recentf-basename-flag nil)
        (firstly-search-recentf-from-start-flag nil))

    (fsr--with-buffer ("/tmp/only.el")
      (save-window-excursion
        (goto-char (point-min))
        (should (= 1 (progn (firstly-search--do-isearch "Click")
                            ;; (isearch-repeat-forward)
                            (point)))) ; return 1 on failure
        (isearch-done)

        (goto-char (point-min))
        (should (= 123 (progn (firstly-search--do-isearch "y.el")
                              (isearch-repeat-forward)
                              (point)))) ; return 1 on failure
        (isearch-done)


        (goto-char (point-min))
        (should (= 120 (progn (firstly-search--do-isearch "only")
                              (isearch-repeat-forward)
                              (point)))) ; return 1 on failure
        (isearch-done)

        (goto-char (point-min))
        (should (= 115 (progn (firstly-search--do-isearch "tmp")
                              (isearch-repeat-forward)
                              (point)))) ; return 1 on failure
        (isearch-done)))))


;; -=-= Tests for `firstly-search-recentf--basename'

(defmacro fsrt--at-line (needle &rest body)
  "Move to first line containing NEEDLE in current buffer, run BODY."
  (declare (indent 1))
  `(progn
     (goto-char (point-min))
     (unless (search-forward ,needle nil t)
       (ert-fail (format "needle %S not found in buffer:\n%s"
                         ,needle (buffer-string))))
     (beginning-of-line)
     ,@body))

(ert-deftest fsrt--basename-absolute ()
  "Absolute recentf entries yield (BASENAME . OFFSET)."
  (fsr--with-buffer ("/home/u/sources/foo.el"
                     "/home/u/sources/bar/baz-tests.el")
    (fsrt--at-line "foo.el"
      (should (equal (car (firstly-search-recentf--basename)) "foo.el")))
    (fsrt--at-line "baz-tests.el"
      (should (equal (car (firstly-search-recentf--basename))
                     "baz-tests.el")))))

(ert-deftest fsrt--basename-tilde ()
  "Tilde-form entries and directory entries work."
  (fsr--with-buffer ("~/sources/firstly-search/"
                     "~/sources/firstly-search/firstly-search-recentf-tests.el")
    (fsrt--at-line "firstly-search/"
      ;; directory: trailing slash must not leak into the basename
      (should (equal (car (firstly-search-recentf--basename))
                     "firstly-search")))
    (fsrt--at-line "recentf-tests.el"
      (should (equal (car (firstly-search-recentf--basename))
                     "firstly-search-recentf-tests.el")))))

(ert-deftest fsrt--basename-offset ()
  "OFFSET points at the basename inside the raw line."
  (fsr--with-buffer ("/tmp/source directoy/"
                     "/tmp/notes/todo.org")
    (dolist (needle '("source directoy" "todo.org"))
      (fsrt--at-line needle
        (let* ((hit  (firstly-search-recentf--basename))
               (line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
          (should (equal (car hit) needle))
          (should (string= (substring line (cdr hit)
                                      (+ (cdr hit) (length (car hit))))
                           (car hit))))))))

;; (fsr--with-buffer ("/tmp/notes/todo.org"
;;                      "/tmp/src/code.el"
;;                      "/tmp/dl/archive.tar.gz")
;;   (goto-char (point-min))
;;   (print (firstly-search-recentf--bounds-normal)))
;;   (should (string-equal (firstly-search-recentf--entries)
;;         "


;; todo.org
;; code.el
;; archive.tar.gz

;; ")))

;; -=-= For `firstly-search-recentf--bounds-normal'

(ert-deftest fsr-bounds-exact-ranges ()
  "Each range slices to exactly the expected basename, in order."
  (fsr--with-buffer ("/tmp/notes/todo.org"
                     "/tmp/src/code.el"
                     "~/code.el"
                     "code.el"
                     "/tmp/source directoy/"
                     "/tmp/dl/archive.tar.gz")
    (let ((res (mapcar (lambda (r)
                      (buffer-substring-no-properties (car r) (cdr r)))
                    (firstly-search-recentf--bounds-normal))))
      ;; res))
    (should
     (equal res
            '("todo.org" "code.el" "code.el" "code.el" "source directoy" "archive.tar.gz"))))))

(ert-deftest fsr-bounds-positions-on-path-lines ()
  "Each range ends at the path tail of the line it sits on."
  (fsr--with-buffer ("/tmp/notes/todo.org"
                     "/tmp/src/code.el"
                     "/tmp/dl/archive.tar.gz")
    (dolist (r (firstly-search-recentf--bounds-normal))
      (save-excursion
        (goto-char (car r))
        (let* ((line (string-trim
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
               (stripped (string-trim
                          (replace-regexp-in-string
                           "\\`\\[[0-9]+\\][ \t]+" "" line)))
               (slice (buffer-substring-no-properties (car r) (cdr r))))
          (should (string-suffix-p slice stripped))
          ;; The range must be the tail of its own line, not just a suffix.
          (should (= (cdr r) (line-end-position)))
          ;; A basename never starts right after a non-'/' char.
          (when (> (car r) (line-beginning-position))
            (should (memq (char-before (car r)) '(?/ ?\s)))))))))

(ert-deftest fsr-bounds-edge-cases ()
  "Spaces in filenames and duplicate basenames each yield a correct range."
  (fsr--with-buffer ("/tmp/with space/a b.org"
                     "/tmp/dup/dup.el"
                     "/tmp/other/dup.el"
                     "~/only/")
    (let ((slices (mapcar (lambda (r)
                            (buffer-substring-no-properties (car r) (cdr r)))
                          (firstly-search-recentf--bounds-normal))))
      (should (member "a b.org" slices))
      (should (equal (seq-filter (lambda (s) (string= s "dup.el")) slices)
                     '("dup.el" "dup.el")))
      ;; Trailing slash on a directory path must not leak into the slice.
      (should (member "only" slices))
      (dolist (s slices)
        (should (and (not (string-empty-p s))
                     (not (string-match-p "/" s))
                     (not (string-match-p "\\s-\\'" s))))))))

(ert-deftest fsr-bounds-offset-matches-basename-start ()
  "Offset equals the index of the basename inside the raw line."
  (fsr--with-buffer ("/tmp/source directoy/"
                     "/tmp/notes/todo.org")
    (dolist (r (firstly-search-recentf--bounds-normal))
      (save-excursion
        (goto-char (car r))
        (let* ((line  (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))
               (slice (buffer-substring-no-properties (car r) (cdr r))))
          (should (= (string-match (concat (regexp-quote slice) "\\(?:/\\|\\'\\)")
                                   line)
                     (- (car r) (line-beginning-position)))))))))

(provide 'firstly-search-recentf-tests)

;;; firstly-search-recentf-tests.el ends here
