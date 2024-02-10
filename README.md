# dired-e-mode
Dired minor mode for Emacs text editor to move cursor by pressing alphabet or number keys.

Fast like with Thunar, Delphin, Windows and MacOS file managers.

Standard Dired keys i, k, d, m should be rebinded before usage.

# to activate
```lisp
(require 'dired-e)
(add-hook 'dired-mode-hook #'dired-e-mode)
```
