;;; auto-dark.el --- Automatically set the dark-mode theme based on MacOS status -*- lexical-binding: t; -*-

;; Author: Rahul M. Juliato
;;         Tim Harper <timcharper at gmail dot com>
;; Created: July 16 2019
;; Version: 0.3
;; Keywords: tools, unix, faces
;; URL: https://github.com/LionyxML/auto-dark-emacs
;; Package-Requires: ((emacs "24.4"))
;; SPDX-License-Identifier: GPL-2.0-only

;;; Commentary:
;; Auto-Dark is an auto-changer between 2 themes, dark/light, respecting the
;; overall settings of MacOS.  To enable it, install the package and add it to your load path:
;;
;;     (require 'auto-dark)
;;
;; To customize the themes used by light / dark mode:
;;
;;     M-x customize-group auto-dark

;;; Code:
(defcustom auto-dark--dark-theme 'wombat
  "The theme to enable when dark-mode is active."
  :group 'auto-dark
  :type 'symbol)

(defcustom auto-dark--light-theme 'leuven
  "The theme to enable when dark-mode is inactive."
  :group 'auto-dark
  :type 'symbol)

(defcustom auto-dark--polling-interval-seconds 5
  "The number of seconds between which to poll for dark mode state.  Emacs must be restarted for this value to take effect."
  :group 'auto-dark
  :type 'integer)

(defcustom auto-dark--allow-osascript nil
  "Whether to allow function `auto-dark-mode' to shell out to osascript to check dark-mode state, if `ns-do-applescript' is not available."
  :group 'auto-dark
  :type 'boolean)

(defvar auto-dark--last-dark-mode-state 'unknown)

(defun auto-dark--is-dark-mode-builtin ()
  "Invoke applescript using Emacs built-in AppleScript support to see if dark mode is enabled.  Return true if it is."

  (string-equal "true" (ns-do-applescript "tell application \"System Events\"
	tell appearance preferences
		if (dark mode) then
			return \"true\"
		else
			return \"false\"
		end if
	end tell
end tell")))

(defun auto-dark--is-dark-mode-osascript ()
  "Invoke applescript using Emacs using external shell command; this is less efficient, but works for non-GUI Emacs."

  (string-equal "true" (string-trim (shell-command-to-string "osascript -e 'tell application \"System Events\" to tell appearance preferences to return dark mode'"))))

(defun auto-dark--is-dark-mode ()
  "If supported, invoke applescript using Emacs built-in AppleScript support to see if dark mode is enabled.  Otherwise, check dark-mode status using osascript, if allowed by auto-dark--allow-osascript."

  (if (fboundp 'ns-do-applescript)
      (auto-dark--is-dark-mode-builtin)

    (and auto-dark--allow-osascript (auto-dark--is-dark-mode-osascript))))

(defun auto-dark--check-and-set-dark-mode ()
  "Set the theme according to Mac OS's dark mode state.  In order to prevent flickering, we only set the theme if we haven't already set the theme for the current dark mode state."
  ;; Get's MacOS dark mode state
  (let ((is-dark-mode (auto-dark--is-dark-mode)))
    (if (not (eq is-dark-mode auto-dark--last-dark-mode-state))
        (progn
          (setq auto-dark--last-dark-mode-state is-dark-mode)
          (if is-dark-mode
              (progn
                (load-theme auto-dark--dark-theme t)
                (disable-theme auto-dark--light-theme))
            (progn
              (load-theme auto-dark--light-theme t)
              (disable-theme auto-dark--dark-theme)))))))

(run-with-timer 0 auto-dark--polling-interval-seconds 'auto-dark--check-and-set-dark-mode)

(provide 'auto-dark)

;;; auto-dark.el ends here
