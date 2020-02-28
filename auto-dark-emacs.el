;;; auto-dark-eamcs.el --- Automatically set the dark-mode state of Emacs

;; Copyright (C) 2019 Rahul M. Juliato

;; Licensed under the same terms as Emacs.

;; Author: Rahul M. Juliato
;;      Tim Harper <timcharper at gmail dot com>
;; Maintainer: Please send bug reports to the github site (below).
;; Created: July 16 2019
;; Version: 0.2
;; Keywords: gui, os-integration
;; URL: https://github.com/LionyxML/auto-dark-emacs
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Auto-Dark is an auto-changer between 2 themes, dark/light, respecting the
;; overall settings of MacOS. To enable it, install the package, add it to your load path, and:
;;
;;     (require 'auto-dark-emacs)
;;
;; To customize the themes used by light / dark mode:
;;
;;     M-x customize-group auto-dark-emacs

;;; Code:

(defcustom auto-dark-emacs/dark-theme 'wombat
  "The theme to enable when dark-mode is active"
  :group 'auto-dark-emacs)

(defcustom auto-dark-emacs/light-theme 'leuven
  "The theme to enable when dark-mode is inactive"
  :group 'auto-dark-emacs)

(defcustom auto-dark-emacs/polling-interval-seconds 5
  "The number of seconds between which to poll for dark mode state. Emacs must be restarted for this value to take effect"
  :group 'auto-dark-emacs
  :type 'integer)

(defcustom auto-dark-emacs/allow-osascript nil
  "Whether to allow auto-dark-mode to shell out to osascript to check dark-mode state, if ns-do-applescript is not available"
  :group 'auto-dark-emacs
  :type 'boolean)

(setq auto-dark-emacs/last-dark-mode-state 'unknown)

(defun auto-dark-emacs/is-dark-mode-builtin ()
  "Invoke applescript using Emacs built-in AppleScript support to see if dark mode is enabled. Returns true if it is."

  (string-equal "true" (ns-do-applescript "tell application \"System Events\"
	tell appearance preferences
		if (dark mode) then
			return \"true\"
		else
			return \"false\"
		end if
	end tell
end tell")))

(defun auto-dark-emacs/is-dark-mode-osascript ()
  "Invoke applescript using Emacs using external shell command; this is less efficient, but works for non-GUI emacs"

  (string-equal "true" (string-trim (shell-command-to-string "osascript -e 'tell application \"System Events\" to tell appearance preferences to return dark mode'"))))

(defun auto-dark-emacs/is-dark-mode ()
  "If supported, invoke applescript using Emacs built-in AppleScript support to see if dark mode is enabled. Otherwise, check dark-mode status using osascript, if allowed by auto-dark-emacs/allow-osascript."

  (if (fboundp 'ns-do-applescript)
      (auto-dark-emacs/is-dark-mode-builtin)

    (and auto-dark-emacs/allow-osascript (auto-dark-emacs/is-dark-mode-osascript))))

(defun auto-dark-emacs/check-and-set-dark-mode ()
  "Sets the theme according to Mac OS's dark mode state. In order to prevent flickering, we only set the theme if we haven't already set the theme for the current dark mode state."
  ;; Get's MacOS dark mode state
  (let ((is-dark-mode (auto-dark-emacs/is-dark-mode)))
    (if (not (eq is-dark-mode auto-dark-emacs/last-dark-mode-state))
        (progn
          (setq auto-dark-emacs/last-dark-mode-state is-dark-mode)
          (if is-dark-mode
              (progn
                (load-theme auto-dark-emacs/dark-theme t)
                (disable-theme auto-dark-emacs/light-theme))
            (progn
              (load-theme auto-dark-emacs/light-theme t)
              (disable-theme auto-dark-emacs/dark-theme)))))))

(run-with-timer 0 auto-dark-emacs/polling-interval-seconds 'auto-dark-emacs/check-and-set-dark-mode)

(provide 'auto-dark-emacs)
