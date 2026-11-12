[![MELPA](https://melpa.org/packages/firstly-search-badge.svg)](https://melpa.org/#/firstly-search)
[![MELPA Stable](https://stable.melpa.org/packages/firstly-search-badge.svg)](https://stable.melpa.org/#/firstly-search)

# firstly-search

Pressing normal keys activates quick isearch navigation. Typing any printable character moves the cursor with incremental search.

Emacs versions: 29.1–31.1

Version: 0.2

Supported modes:

- For Dired File manager — Modern navigation like in Thunar, Delphin, Windows and MacOS file managers.

- For Package Menu — fast search in package names.

- For Buffer Menu — fast selection of buffer by name.

- For Recentf Menu — fast search in basename of paths.

- For Ibuffer — fast search.

This is minor mode for Emacs text editor. No external dependencies required.

Pay attention, standard keys: **i**, **k**, **d**, **m** is rebinded to **M-** and **C-M-** prefix, by default. You may rebind manually.


## Features
- support for modes: Dired, Package Menu, Buffer Menu.
- **C-n** and **C-p** is used during searching as **C-s** isearch-forward and **C-r** isearch-backward commands.
- fast switching direction of moving, cancel navigation at failure
- any printable character activate isearch-navigation. Modifiers used for commands.
- **C-m** or **RET** quit search and allow quickly select item.
- disable itself in Editable Dired mode (wdired-mode).


## Activation
Add this lines to your configuration Init file: ```~/.emacs```, ```~/.emacs.d/init.el```, ```~/.config/emacs/init.el```:

Load for required major modes:
```lisp
(add-to-list 'load-path (expand-file-name "~/sources/firstly-search")) ; optional: for installation from sources
(require 'firstly-search-dired)
(require 'firstly-search-package)
(require 'firstly-search-buffermenu)
(require 'firstly-search-bookmarks)
(require 'firstly-search-recentf)
(require 'firstly-search-ibuffer)
(add-hook 'dired-mode-hook		#'firstly-search-dired-mode)
(add-hook 'package-menu-mode-hook	#'firstly-search-package-mode)
(add-hook 'Buffer-menu-mode-hook	#'firstly-search-buffermenu-mode)
(add-hook 'bookmark-bmenu-mode-hook	#'firstly-search-bookmarks-mode)
(add-hook 'recentf-dialog-mode-hook	#'firstly-search-recentf-mode)
(add-hook 'ibuffer-mode-hook		#'firstly-search-ibuffer-mode)
```

Note, recommende way to open recentf:
```lisp
(defun my/recentf-open-files ()
  (interactive)
  (recentf-load-list)
  (recentf-open-files))
(global-set-key (kbd "M-r") #'my/recentf-open-files)
(setopt recentf-max-saved-items 50)
```

## Customization

```lisp
M-x customize-group RET firstly-search
M-x customize-group RET firstly-search-buffermenu
M-x customize-group RET firstly-search-package
M-x customize-group RET firstly-search-dired
M-x customize-group RET firstly-search-recentf
M-x customize-group RET firstly-search-ibuffer
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

## How this works
We use `pre-command-hook' called for any key pressed, if key is in `firstly-search-keys' we activate configured incremental search (isearch). Also we correctly handle failed search and set timer.  To prevent intererence with major-mode keybindings we create copy of keymap with :parent of major-mode map at loading. At minor mode activation we set our map copy and set hooks.

## Other packages
- Navigation in Dired, Packages, Buffers modes https://github.com/Anoncheg1/firstly-search
- Search with Chinese		https://github.com/Anoncheg1/pinyin-isearch
- Ediff fix			https://github.com/Anoncheg1/ediffnw
- Dired history		https://github.com/Anoncheg1/dired-hist
- Selected window contrast	https://github.com/Anoncheg1/selected-window-contrast
- Copy link to clipboard	https://github.com/Anoncheg1/org-links
- Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
- Restore buffer state		https://github.com/Anoncheg1/emacs-unmodified-buffer1
- outline.el usage		https://github.com/Anoncheg1/emacs-outline-it
- Call LLMs and AI agents from Org-mode ai block. https://github.com/Anoncheg1/emacs-cui
- TAB key reimplementation	https://github.com/Anoncheg1/emacs-indent

## You may sponsor the author directly with cryptocurrencies

- BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25

![BTC](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/BTC-1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25.png)

- USDT (Tether TRX-TRON) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN

![BTC](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/USDT-TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN.png)

- TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D