;;; auto-dark-initialize.el --- Functions to simulate Emacs’ initialization  -*- lexical-binding: t; -*-

;; Author: Greg Pfeil <greg@technomadic.org>
;; Created: October 11 2024
;; Version: 0.1.0
;; Keywords: maint, tools
;; URL: https://github.com/LionyxML/auto-dark-emacs
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; This library attempts to simulate the Emacs initialization process, for
;; writing tests that are sensitive to initialization.

;;; Code:

(defvar auto-dark-initialize--current-state nil
  "The phase of initialization we’ve gotten to.
So users don’t have to call every function.")

(defun auto-dark-initialize-start ()
  "Set up various things to a state similar to what existed before early init."
  (if auto-dark-initialize--current-state
      (lwarn 'auto-dark-initialize :warn
             "Initialization is already at least as far as 'start, not running.")
    (setq after-init-time nil
          auto-dark-initialize--current-state 'start)))

(defun auto-dark-initialize-after-early-init (autoload-features)
  "Update state as done between early init and the rest of the init files.
E.g., initializing the package system and setting up the display.
AUTOLOAD-FEATURES should be a list of symbols naming the autoload features for
packages you are testing."
  (unless auto-dark-initialize--current-state
    (auto-dark-initialize-start))
  (if (eq auto-dark-initialize--current-state 'start)
      (progn
        (mapc (lambda (feature) (require feature)) autoload-features)
        (custom-set-variables
         '(frame-background-mode 'light))
        (setq auto-dark-initialize--current-state 'early-init))
    (lwarn 'auto-dark-initialize :warn
           "Initialization is already at least as far as 'early-init, not running.")))

(defun auto-dark-initialize-finish ()
  "This is the rest of the init process after the various files are loaded.
After this is run, the tests should reflect a “normal” Emacs session."
  (pcase auto-dark-initialize--current-state
    ('(nil start)
     (signal 'auto-dark-initialize
             '("At least ‘auto-dark-initialize-after-early-init needs to be called first.’")))
    ('early-init
     (setq after-init-time (current-time))
     (run-hooks 'after-init-hook)
     (setq auto-dark-initialize--current-state 'finished))
    (_ (lwarn 'auto-dark-initialize :warn
              "Initialization has already finished, not running."))))

(provide 'auto-dark-initialize)
;;; auto-dark-initialize.el ends here
