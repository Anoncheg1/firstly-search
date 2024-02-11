# dired-e-mode
Modern navigation for file manager.

Dired minor mode for Emacs text editor to move cursor by pressing alphabet or number keys.

Fast like with Thunar, Delphin, Windows and MacOS file managers.

Standard Dired keys i, k, d, m should be rebinded before usage.

# difference with dired-explorer.el package
- high-level dired-isearch-filenames used instead of low-level re-search-forward and re-search-backward.
- more customization and accurate navigation


# to activate
```lisp
(require 'dired-e)
(add-hook 'dired-mode-hook #'dired-e-mode)
```
