# Auto-Dark for Emacs


[![MELPA](https://melpa.org/packages/auto-dark-badge.svg)](https://melpa.org/#/auto-dark)


Do you want Emacs to follow your MacOS/Linux/Windows/Android dark-mode on/off
options?


This package, `auto-dark-mode`, introduces a minor mode in Emacs that
enables automatic switching between two user-defined (customizable)
themes. This transition occurs seamlessly in response to Dark Mode
being enabled or disabled across MacOS, Linux, Windows, and Android
platforms.


For now it supports Linux through `dbus` and Android via `Termux`.


## Installation

### Regular Emacs

Install it from [MELPA](https://melpa.org/#/auto-dark) and add to your
`.emacs` or `init.el` file:


```emacs-lisp
(require 'auto-dark)
(auto-dark-mode t)
```


Or simply copy the `auto-dark.el` file to
`~/.emacs.d/auto-dark/auto-dark.el` (or clone this repository there),
and then add the following to your `.emacs`:


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

If you use Spacemacs, add `(auto-dark)` to the
`dotspacemacs-additional-packages` list and add the following to
`dotspacemacs/user-config`:


```emacs-lisp
(use-package auto-dark
  :init (spacemacs/defer-until-after-user-config (lambda () (auto-dark-mode t)))
  :defer t)
```

This ensures that `auto-dark-mode` is activated only after spacemacs's
built-in theme loading logic.


### Doom Emacs

If you're under Doom Emacs, the following configuration should be
enough:


```emacs-lisp
;; In your packages.el
(package! auto-dark)

;; In your config.el

(after! doom-ui
  ;; set your favorite themes
  (setq! auto-dark-dark-theme 'doom-one
        auto-dark-light-theme 'doom-one-light)
  (auto-dark-mode 1))
```


## Notes for MacOS users

From the box, this package takes advantage of some built-in functionality found
on the formulaes [Emacs Plus](https://github.com/d12frosted/homebrew-emacs-plus) 
and [Emacs Mac](https://github.com/railwaycat/homebrew-emacsmacport?tab=readme-ov-file)
to make detecting switches faster.


If you compiled Emacs yourself or used any other pre-compiled binary,
it is essential to explicitly instruct `auto-dark` you want to use
`Osascript`.


You can do this by adding to your configuration:


```emacs-lisp
(setq auto-dark-allow-osascript t)
```


Doing so will probably make MacOS prompt you for security permissions.
If by any chance it does not prompt you, you can check permissions on MacOS
by going to:


```
Settings -> Privacy & Security -> Emacs -> System Events 
```


Also notice if you run emacs from  the terminal, `Osascript` is the only method that
will work.


## Settings

All provided options, including The light/dark themes can be
customized using the Emacs customization system. `M-x customize-group auto-dark RET`.


You can also take advantage of the hooks `auto-dark-dark-mode-hook`
and `auto-dark-light-mode-hook` to make it even further
customizable. Take a look at this article on how to [Integrate
Catppuccin with
Auto-Dark](https://www.rahuljuliato.com/posts/auto-dark-catppuccin).


Following, a complete configuration with all settings set to its defaults:

```emacs-lisp
(use-package auto-dark
  :ensure t
  :config 
  (setq auto-dark-dark-theme 'wombat)
  (setq auto-dark-light-theme 'leuven)
  (setq auto-dark-polling-interval-seconds 5)
  (setq auto-dark-allow-osascript nil)
  (setq auto-dark-allow-powershell nil)
  ;; (setq auto-dark-detection-method nil) ;; dangerous to be set manually

  (add-hook 'auto-dark-dark-mode-hook
    (lambda ()
      ;; something to execute when dark mode is detected))

  (add-hook 'auto-dark-light-mode-hook
    (lambda ()
      ;; something to execute when light mode is detected))

  (auto-dark-mode t))
```


A short description of each setting:


#### `auto-dark-dark-theme`

The theme to enable when dark-mode is active.


Possible values are themes installed on your system found by
`customize-themes` or `nil` to use Emacs with no themes (default
appearance).


#### `auto-dark-light-theme`

The theme to enable when dark-mode is inactive.


Possible values are themes installed on your system found by
`customize-themes` or `nil` to use Emacs with no themes (default
appearance).


#### `auto-dark-polling-interval-seconds`

The number of seconds between which to poll for dark mode state.
Emacs must be restarted for this value to take effect.


This is here for when there's no emacs-plus (MacOS), or emacs-mac
(MacOS) or a system with dbus or capable of sending events is found, a
timed polling is called to check the current system status.


#### `auto-dark-allow-osascript`

Whether to allow function `auto-dark-mode` to shell out to osascript:
to check dark-mode state, if `ns-do-applescript` or `mac-do-applescript`.


This is only useful for MacOS, please check the section `Notes for
MacOS users` above.


#### `auto-dark-allow-powershell`

Whether to allow function `auto-dark-mode` to shell out to powershell:
to check dark-mode state.


This is only useful for `Windows`. If not set, it will use the built-in Emacs
Windows Registry functions.


#### `auto-dark-dark-mode-hook`

List of hooks to run after dark mode is loaded.


You can use this hook to take leverage of `auto-dark` detection system and
issue more elisp code when some state is detected. You can even use **only** the
hooks by setting the themes to `nil`.


#### `auto-dark-light-mode-hook`

"List of hooks to run after light mode is loaded."


You can use this hook to take leverage of `auto-dark` detection system and
issue more elisp code when some state is detected. You can even use **only** the
hooks by setting the themes to `nil`.


#### `auto-dark-detection-method`

The method auto-dark should use to detect the system theme.


Defaults to nil and will be populated through feature detection
if left as such. Only set this variable if you know what you're
doing!


## Screenshots

This package in action:

- macOS (emacs-plus formulae)

![auto-dark-emacs in action - macos - emacs-plus](images/demo_emacs_plus.gif)

- macOS (emacs-mac formulae)

![auto-dark-emacs in action - macos - emacs-mac](images/demo_emacs_mac.gif)

- Linux (Gnome DE)

![auto-dark-emacs in action - linux gnome](images/demo_gnome.gif)

