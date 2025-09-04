[![MELPA](https://melpa.org/packages/firstly-search-badge.svg)](https://melpa.org/#/firstly-search)
[![MELPA Stable](https://stable.melpa.org/packages/firstly-search-badge.svg)](https://stable.melpa.org/#/firstly-search)

Tested with Emacs 30.1

# firstly-search

Typing any printable character moves cursor with incremental search. Supported modes:

- For Dired File manager - Modern navigation like in Thunar, Delphin, Windows and MacOS file managers.

- For Package Menu - fast search in package names.

- For Buffer Menu - fast selection of buffer by name.

This is minor mode for Emacs text editor. No external dependencies required.

Pay attention, standard keys: **i**, **k**, **d**, **m** is rebinded to **M-** and **C-M-** prefix, by default. You may rebind manually.

## Features
- support for modes: Dired, Package Menu, Buffer Menu.
- **C-n** and **C-p** is used during searching as **C-s** isearch-forward and **C-r** isearch-backward commands.
- any printable character activate isearch-navigation. Modifiers used for commands.
- **C-m** or **RET** quit search and allow quickly select item.
- disable itself in Editable Dired mode (wdired-mode).

## Activation
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

## Customization

```lisp
M-x customize-group RET firstly-search
M-x customize-group RET firstly-search-buffermenu
M-x customize-group RET firstly-search-package
M-x customize-group RET firstly-search-dired
```

## demo
![Demo](https://codeberg.org/Anoncheg/public-share/raw/branch/main/dired-e.gif)

## Note
I recommend to highlight current line.
```lisp
(add-hook 'dired-mode-hook #'hl-line-mode)
```
## Another implementation

- dired-explorer.el package have the same functionality for Dired mode.
- 2009 dired-lis https://github.com/weikent/emacs/blob/master/lisps/dired/dired-lis.el or http://code.google.com/p/dea/source/browse/trunk/my-lisps/dired-lis.el

## known bugs
- in Buffer Menu C-g do not break search completely.

## Donate crypto, sponsor author:
- BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
- USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
