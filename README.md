Auto-Dark-Emacs
---------------

Wanna emacs to follow your MacOS Dark-mode on/off options?

That's it. This program lets Emacs change between 2 user defined (in code) themes to be automatically changed when Dark Mode set on/off on MacOS.

By default, themes are wombat and leuven, since these are packed together with Emacs.


Install
-------

Simply copy the auto-dark-emacs.el file to `~/.emacs.d/vendor/auto-dark-emacs/auto-dark-emacs.el` (or clone this repository there), and then add the following to your `.emacs`:

```
(add-to-list 'load-path "~/.emacs.d/vendor/auto-dark-emacs/")
(require 'auto-dark-emacs)
```

Usage
-----
Change your dark-mode settings on MacOS and let the magic happens :D

Customization
-----
The light/dark themes can be customized using the Emacs customization system. `M-x customize-group auto-dark-emacs`.

OSA Script fallback
-----
For terminal-based emacs, it is possible to check dark mode status using osascript rather than relying on the built-in Applescript support that GUI Emacs provides. To enable it, customize `dark-mode-emacs/allow-osascript` and set it to `t`.

Screenshot
----------
Here it's what it does.

![auto-dark-emacs  in action](images/ade.gif)


