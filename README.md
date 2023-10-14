# Auto-Dark for Emacs

[![MELPA](https://melpa.org/packages/auto-dark-badge.svg)](https://melpa.org/#/auto-dark)

Do you want Emacs to follow your MacOS/Linux/Windows Dark-mode on/off options?

This is it. This program lets Emacs change between 2 user defined (customizable) themes to be automatically changed when Dark Mode set on/off on MacOS/Linux/Windows. For now it supports Linux through Gnome and Termux(Android).

By default, themes are _wombat_ and _leuven_, since these are bundled with Emacs.

## Install

Install it from [MELPA](https://melpa.org/#/auto-dark) and add to your `.emacs` file:

```emacs-lisp
(require 'auto-dark)
(auto-dark-mode t)
```

Or simply copy the auto-dark.el file to `~/.emacs.d/auto-dark/auto-dark.el` (or clone this repository there), and then add the following to your `.emacs`:

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/auto-dark/")
(require 'auto-dark)
(auto-dark-mode t)
```

Or use `use-package` to install:

```emacs-lisp
(use-package auto-dark
  :config (auto-dark-mode t))
```

### Spacemacs

If you use Spacemacs, add `(auto-dark)` to the `dotspacemacs-additional-packages` list and add the following to `dotspacemacs/user-config`:
```emacs-lisp
(use-package auto-dark
  :init (spacemacs/defer-until-after-user-config (lambda () (auto-dark-mode t)))
  :defer t)
```
This ensures that `auto-dark-mode` is activated only after spacemacs's built-in theme loading logic.

### Doom Emacs 
If you use Doom Emacs, the following config should do the trick

```emacs-lisp
;; In your packages.el
(package! auto-dark)

;; In your config.el

(after! doom-themes
  ;; set  your favorite themes
  (setq! auto-dark-dark-theme 'doom-one
        auto-dark-light-theme 'doom-one-light)
  (auto-dark-mode 1))
```

## Usage

Change your dark-mode settings on MacOS/Linux/Windows and let the magic happens :D

## Customization

The light/dark themes can be customized using the Emacs customization system. `M-x customize-group auto-dark`.

You can also take advantage of the hooks `auto-dark-dark-mode-hook`
and `auto-dark-light-mode-hook` to make it even further
customizable. Take a look at this article on how to [Integrate
Catppuccin with
Auto-Dark](https://www.rahuljuliato.com/posts/auto-dark-catppuccin).


## OSA Script fallback (macOS)

For terminal-based emacs, it is possible to check dark mode status using osascript rather than relying on the built-in Applescript support that GUI Emacs provides. To enable it, customize `dark-mode-allow-osascript` and set it to `t`.

## Screenshot

This package in action:

- macOS

![auto-dark-emacs in action - macos](images/demo.gif)


- Linux (Gnome DE)

![auto-dark-emacs in action - linux gnome](images/demo_gnome.gif)

