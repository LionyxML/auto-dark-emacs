# Auto-Dark for Emacs

[![MELPA](https://melpa.org/packages/auto-dark-badge.svg)](https://melpa.org/#/auto-dark)

Do you want Emacs to follow your MacOS/Linux/Windows Dark-mode on/off options?

This is it. This program lets Emacs change between 2 user defined (customizable) themes to be automatically changed when Dark Mode set on/off on MacOS/Linux/Windows. Now only supports Gnome on Linux.

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
  :init (auto-dark-mode t))
```

## Usage

Change your dark-mode settings on MacOS/Linux/Windows and let the magic happens :D

## Customization

The light/dark themes can be customized using the Emacs customization system. `M-x customize-group auto-dark`.

## OSA Script fallback

For terminal-based emacs, it is possible to check dark mode status using osascript rather than relying on the built-in Applescript support that GUI Emacs provides. To enable it, customize `dark-mode-allow-osascript` and set it to `t`.

## Screenshot

A demo of this package in action.

![auto-dark-emacs in acition full screen](https://github.com/LionyxML/auto-dark-emacs/blob/docs/maintenance/images/demo.mp4?raw=true)
