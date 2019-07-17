;; -*- lexical-binding: t -*-
;; Auto-Dark-Emacs is an auto changer between 2 themes, dark/light, respecting the
;; overall settings of MacOS
(require 'timer)


(defcustom auto-dark-dark-theme 'wombat
  "Theme for the dark mode state of auto-dark-mode.")

(defcustom auto-dark-light-theme 'wombat
  "Theme for the light mode state of auto-dark-mode.")

(defvar auto-dark-state nil
  "The current state of auto-dark-mode.")

(defconst auto-dark-get-state-command
  "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\""
  "Command to run to find the light/dark mode of MacOS.")


(defun auto-dark-mode-set-state (state)
  "Set the theme to either dark or light, accoring to `state'."
  (setf auto-dark-state state)
  (cond
   ((eq state 'dark)
	(disable-theme auto-dark-light-theme)
	(load-theme auto-dark-dark-theme))
   ((eq state 'light)
	(disable-theme auto-dark-dark-theme)
	(load-theme auto-dark-light-theme))))


(defun auto-dark-dark-p ()
  "Determine whether or not MacOS is using dark mode."
  (string= (shell-command-to-string auto-dark-get-state-command) "true"))


(defun auto-dark-update ()
  (let ((state (if (auto-dark-dark-p)
				   'dark
				 'light)))
	(unless (eq state auto-dark-state)
	  (auto-dark-set-state))))


(defvar auto-dark-timer nil
  "Timer for updating according to the state of MacOS.")


(define-minor-mode auto-dark-mode
  "Mode for auto-updating Emacs' theme based on MacOS light/dark state."
  1
  nil
  nil
  :global t
  (setf auto-dark-timer (run-with-timer 0 1 #'auto-dark-update)))


(provide 'auto-dark)
