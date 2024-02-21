![Eask badge](https://github.com/Anoncheg1/dired-e/actions/workflows/test.yml/badge.svg?event=release)
![melpazoid badge](https://github.com/Anoncheg1/dired-e/actions/workflows/melpazoid.yml/badge.svg)

Tested with Emacs 29.1

# firstly-search
Modern navigation like in Thunar, Delphin, Windows and MacOS file managers.

Typing any printable character activate incremental search in file names.

This is Dired minor mode for Emacs text editor. No external dependencies required.

Standard Dired keys i, k, d, m should be rebinded before usage.

# Features, difference with dired-explorer.el package
- any printable character activate isearch-navigation. Modifiers used for commands.
- high-level dired-isearch-filenames is used instead of low-level re-search-forward and re-search-backward.
- more customization and accurate navigation

# Activation
Add this lines to your configuration (/home/user/.emacs):
```lisp
(require 'dired-fs)
(add-hook 'dired-mode-hook #'dired-fs-mode)
```

# demo
![Demo](https://github.com/Anoncheg1/public-share/blob/main/dired-e.gif)
