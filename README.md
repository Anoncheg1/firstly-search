![example workflow](https://github.com/Anoncheg1/dired-e-mode/actions/workflows/test.yml/badge.svg?event=release)

# dired-e-mode
Modern navigation like in Thunar, Delphin, Windows and MacOS file managers.

Dired minor mode for Emacs text editor to move cursor by typing name of file.

Standard Dired keys i, k, d, m should be rebinded before usage.

# difference with dired-explorer.el package
- high-level dired-isearch-filenames is used instead of low-level re-search-forward and re-search-backward.
- more customization and accurate navigation


# to activate
add this lines to your configuration (/home/user/.emacs)
```lisp
(require 'dired-e)
(add-hook 'dired-mode-hook #'dired-e-mode)
```

# demo
![Demo](https://github.com/Anoncheg1/public-share/blob/main/dired-e.gif)
