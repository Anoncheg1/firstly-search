;;; firstly-search-dired-tests.el --- Tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'dired)
(require 'firstly-search)
(require 'firstly-search-dired)

;;; Code:

(ert-deftest firstly-search-dired-activation-and-idle-renewal ()
  "No-stub test: real isearch loop, idle timer aborts it.

Observers latch the first time they see the flag `t'; later
updates (including the one fired from inside `isearch-done')
cannot un-latch them."
  (skip-unless (fboundp 'keymap-set))

  (let* ((dir   (make-temp-file "firstly-search-test-" t))
         (files '("alpha.txt" "beta.txt" "gamma.txt")))
    (unwind-protect
        (progn
          (dolist (f files)
            (with-temp-file (expand-file-name f dir) (insert "hello")))
          (dired dir)
          (let ((buf (current-buffer)))
            (unwind-protect
                (with-current-buffer buf
                  (setq-local firstly-search-renew-idle-seconds 0.05)
                  (setq-local firstly-search--isearch-navigation-flag nil)
                  (setq-local isearch-mode nil)
                  (when (timerp firstly-search--renew-timer)
                    (cancel-timer firstly-search--renew-timer)
                    (setq firstly-search--renew-timer nil))
                  (firstly-search-dired-mode 1)

                  (should firstly-search-dired-mode)
                  (should-not firstly-search--isearch-navigation-flag)

                  ;; Latching observers: once we see `t', stay `t'.
                  (let ((saw-isearch nil)
                        (saw-flag   nil)
                        (updates    0))
                    (add-hook 'isearch-update-post-hook
                              (lambda ()
                                (setq updates (1+ updates))
                                (when isearch-mode
                                  (setq saw-isearch t))
                                (when firstly-search--isearch-navigation-flag
                                  (setq saw-flag t)))
                              nil t)

                    ;; Feed exactly one key; the idle timer aborts isearch.
                    (setq unread-command-events (list ?a))
                    (execute-kbd-macro (kbd "a"))

                    ;; In case the timer did not fire, exit cleanly.
                    (when isearch-mode (isearch-done))

                    (should (> updates 0))
                    (should saw-isearch)
                    (should saw-flag))

                  ;; After abort: everything cleared.
                  (should-not isearch-mode)
                  (should-not firstly-search--isearch-navigation-flag))
              (when (buffer-live-p buf) (kill-buffer buf)))))
      (when (file-directory-p dir) (delete-directory dir t)))))


(ert-deftest firstly-search-dired-activation-not-activate ()
  "No-stub test: real isearch loop, idle timer aborts it.

Observers latch the first time they see the flag `t'; later
updates (including the one fired from inside `isearch-done')
cannot un-latch them."
  (skip-unless (fboundp 'keymap-set))

  (let* ((dir   (make-temp-file "firstly-search-test-" t))
         (files '("alpha.txt" "beta.txt" "gamma.txt")))
    (unwind-protect
        (progn
          (dolist (f files)
            (with-temp-file (expand-file-name f dir) (insert "hello")))
          (dired dir)
          (let ((buf (current-buffer)))
            (unwind-protect
                (with-current-buffer buf
                  ;; (setq-local firstly-search-renew-idle-seconds 0.05)
                  (setq-local firstly-search--isearch-navigation-flag nil)
                  (setq-local isearch-mode nil)
                  (when (timerp firstly-search--renew-timer)
                    (cancel-timer firstly-search--renew-timer)
                    (setq firstly-search--renew-timer nil))


                  (should (eq (key-binding "m") 'dired-mark)) ; check map change

                  (firstly-search-dired-mode 1)

                  (should (eq (key-binding "m") 'undefined)) ; check map change

                  (should firstly-search-dired-mode)
                  (should-not firstly-search--isearch-navigation-flag)

                  ;; Latching observers: once we see `t', stay `t'.
                  (let ((saw-isearch nil)
                        (saw-flag   nil)
                        (updates    0))
                    (add-hook 'isearch-update-post-hook
                              (lambda ()
                                (setq updates (1+ updates))
                                (when isearch-mode
                                  (setq saw-isearch t))
                                (when firstly-search--isearch-navigation-flag
                                  (setq saw-flag t)))
                              nil t)

                    ;; Feed exactly one key; the idle timer aborts isearch.
                    ;; (setq unread-command-events (list ?a))
                    ;; (push ?# unread-command-events)
                    (execute-kbd-macro (kbd "#"))
                    ;; (execute-kbd-macro (string-to-vector "a") nil)

                    ;; In case the timer did not fire, exit cleanly.
                    (when isearch-mode (isearch-done))

                    (should-not (> updates 0))
                    (should-not saw-isearch)
                    (should-not saw-flag)


                    ;; (print (list firstly-search-dired-mode
                    ;;              firstly-search--isearch-navigation-flag
                    ;;              isearch-mode)))))))))
;; ))))))))
                    (execute-kbd-macro (kbd "a"))

                    (when isearch-mode (isearch-done))

                    (should (> updates 0))
                    (should saw-isearch)
                    (should saw-flag)
                    (setq saw-isearch nil
                          saw-flag   nil
                          updates    0)
                    )

                  ;; After abort: everything cleared.
                  (should-not isearch-mode)
                  (should-not firstly-search--isearch-navigation-flag))
              (when (buffer-live-p buf) (kill-buffer buf)))))
      (when (file-directory-p dir) (delete-directory dir t)))))


(ert-deftest firstly-search-dired-keys-mapping ()
  (let ((dir   (make-temp-file "firstly-search-test-" t)))
    (unwind-protect
        (remove-hook 'dired-mode-hook	#'firstly-search-dired-mode)
        (dired dir)
          (let ((buf (current-buffer)))
            (unwind-protect
                (with-current-buffer buf

                  (should (eq (key-binding "m") 'dired-mark)) ; check map change
                  (should (eq (key-binding (kbd "M-i")) 'tab-to-tab-stop))

                  (firstly-search-dired-mode 1)

                  (should (eq (key-binding "m") 'undefined)) ; check map change
                  (should (eq (key-binding (kbd "M-i")) 'dired-maybe-insert-subdir))

                  (firstly-search-dired-mode -1)

                  (should (eq (key-binding "m") 'dired-mark))
                  (should (eq (key-binding (kbd "M-i")) 'tab-to-tab-stop))))))))


(ert-deftest firstly-search-dired-idle-timer-aborts-isearch ()
  "Real isearch; real idle timer; BEFORE/AFTER asserted around `sit-for'."
  (skip-unless (fboundp 'keymap-set))

  (let ((dir (make-temp-file "firstly-search-test-" t))
        (before-ok nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "alpha.txt" dir) (insert "x"))
          (dired dir)
          (let ((buf (current-buffer)))
            (unwind-protect
                (with-current-buffer buf
                  (setq-local firstly-search-renew-idle-seconds 0.05)
                  (setq-local firstly-search--isearch-navigation-flag nil)
                  (setq-local isearch-mode nil)
                  (when (timerp firstly-search--renew-timer)
                    (cancel-timer firstly-search--renew-timer)
                    (setq firstly-search--renew-timer nil))
                  (firstly-search-dired-mode 1)

                  (add-hook 'isearch-update-post-hook
                            (lambda ()
                              (when (and isearch-mode
                                         firstly-search--isearch-navigation-flag
                                         (not before-ok))
                                (setq before-ok t)
                                ;; BEFORE: isearch is active, nav flag set,
                                ;; timer armed.  Assert, then let the timer
                                ;; fire during the real idle window.
                                (unless (timerp firstly-search--renew-timer)
                                  (ert-fail "renew timer not armed"))
                                (sit-for
                                 (* 3 firstly-search-renew-idle-seconds))))
                            nil t)


                  ;; (setq unread-command-events (list ?a))
                  (execute-kbd-macro (kbd "a"))
                  (when isearch-mode (isearch-done))

                  ;; --- AFTER ---
                  (should before-ok)
                  (should-not isearch-mode)
                  (should-not firstly-search--isearch-navigation-flag)
                  (should-not firstly-search--renew-timer)
                  (should firstly-search-dired-mode))
              (when (buffer-live-p buf) (kill-buffer buf)))))
      (when (file-directory-p dir) (delete-directory dir t)))))

(provide 'firstly-search-dired-tests)

;;; firstly-search-dired-tests.el ends here
