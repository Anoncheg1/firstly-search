![Eask badge](https://github.com/Anoncheg1/dired-e/actions/workflows/test.yml/badge.svg?event=release)
![melpazoid badge](https://github.com/Anoncheg1/dired-e/actions/workflows/melpazoid.yml/badge.svg)

Tested with Emacs 29.1

# firstly-search

Typing any printable character moves cursor with incremental search. Supported modes:

- For Dired File manager - Modern navigation like in Thunar, Delphin, Windows and MacOS file managers.

- For Package Menu - fast search in package names.

- For Buffer Menu - fast selection of buffer by name.

This is minor mode for Emacs text editor. No external dependencies required.

Pay attention, standard keys: **i**, **k**, **d**, **m** is rebinded to **M-** and **C-M-** prefix, by default. You may rebind manually.

# Features
- Support for modes: Dired, Package Menu, Buffer Menu.
- **C-n** and **C-p** is used during searching as **C-s** isearch-forward and **C-r** isearch-backward commands.
- any printable character activate isearch-navigation. Modifiers used for commands.
- **C-m** or **RET** quit search and allow quickly select item.
- allow Editable Dired mode (wdired-mode).

# Activation
Add this lines to your configuration Init file: ```~/.emacs```, ```~/.emacs.d/init.el```, ```~/.config/emacs/init.el```:

```lisp
;; Dired
(require 'firstly-search-dired)
(add-hook 'dired-mode-hook #'firstly-search-dired-mode)
;; Package menu
(require 'firstly-search-package)
(add-hook 'package-menu-mode-hook #'firstly-search-package-mode)
;; Buffer Menu
(require 'firstly-search-buffermenu)
(add-hook 'Buffer-menu-mode-hook #'firstly-search-buffermenu-mode)
;; Bookmarks
(require 'firstly-search-bookmarks)
(add-hook 'bookmark-bmenu-mode-hook #'firstly-search-bookmarks-mode)
```

# Customization

```lisp
M-x customize-group RET firstly-search
M-x customize-group RET firstly-search-buffermenu
M-x customize-group RET firstly-search-package
M-x customize-group RET firstly-search-dired
```

# demo
![Demo](https://github.com/Anoncheg1/public-share/blob/main/dired-e.gif)

# Notes

dired-explorer.el package have the same functionality for Dired mode.
