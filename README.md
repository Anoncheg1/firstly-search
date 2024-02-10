# dired-e-mode
Dired minor mode to move cursor by just pressing alphabet or number keys.

Standard keys i, k, d, m should be rebinded before usage.

# to activate
```lisp
(require 'dired-e)
(add-hook 'dired-mode-hook #'dired-e-mode)
```
