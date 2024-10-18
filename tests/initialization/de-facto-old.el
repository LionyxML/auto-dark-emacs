;;; de-facto-old.el --- Tests that need to be run in a fresh instance -*- lexical-binding: t; -*-

;;; Commentary:

;; This models the most common initialization style of Emacs, where Customize
;; inserts a `custom-set-variables' form at the end of `user-init-file'.

;; 1. `package-initialize' is called, then
;; 2. `user-init-file' is loaded, which has
;;   a. some initialization of `auto-dark-mode' and
;;   b. `custom-set-variables' at the end.

;;; Code:

(require 'auto-dark-initialize)
(require 'buttercup)

;; To silence “reference to free variable” warnings
(defvar auto-dark-allow-osascript)
(defvar auto-dark-allow-powershell)
(defvar auto-dark-dark-theme)
(defvar auto-dark-light-theme)
(defvar auto-dark-polling-interval-seconds)
(defvar auto-dark-themes)

(describe "Emacs’s de-facto initialization with old theme vars"
  (before-all
    (auto-dark-initialize-after-early-init '(auto-dark-autoloads)))

  (describe "after ‘package-initialize’ is called"
    (it "should not have enabled the themes yet"
      (expect custom-enabled-themes :to-be ()))
    (it "should not have bound Auto-Dark variables"
      (expect (boundp 'auto-dark-allow-osascript) :to-be nil)
      (expect (boundp 'auto-dark-allow-powershell) :to-be nil)
      (expect (boundp 'auto-dark-dark-theme) :to-be nil)
      (expect (boundp 'auto-dark-detection-method) :to-be nil)
      (expect (boundp 'auto-dark-light-theme) :to-be nil)
      (expect (boundp 'auto-dark-polling-interval-seconds) :to-be nil)
      (expect (boundp 'auto-dark-themes) :to-be nil)))

  (describe "before ‘custom-set-variables’ is called"
    (before-all
      (auto-dark-mode 1))

    (it "should not have enabled the themes yet"
      (expect custom-enabled-themes :to-equal ()))
    (it "should have bound Auto-Dark variables to their standard values"
      (expect auto-dark-allow-osascript :to-be nil)
      (expect auto-dark-allow-powershell :to-be nil)
      (expect (boundp 'auto-dark-detection-method) :to-be-truthy)
      (expect auto-dark-polling-interval-seconds :to-be 5)
      (expect auto-dark-themes :to-be nil)
      ;; These two are handled specially – they aren’t set to their defaults
      ;; until after initialization.
      (expect (boundp 'auto-dark-dark-theme) :to-be nil)
      (expect (boundp 'auto-dark-light-theme) :to-be nil)))

  (describe "after vars are set"
    (before-all
      (custom-set-variables
       '(auto-dark-dark-theme 'tsdh-dark)
       '(auto-dark-light-theme 'tsdh-light)))

    ;; NB: When using the old variables, they won’t be set until
    ;;    `after-init-hook' is run.
    (it "shouldn’t have changed anything yet"
      (expect (boundp 'auto-dark-dark-theme) :to-be nil)
      (expect (boundp 'auto-dark-light-theme) :to-be nil)))

  (describe "after ‘user-init-file’ is loaded"
    (before-all
      (auto-dark-initialize-finish))

    (it "should have configured Auto-Dark"
      (expect auto-dark-themes :to-be nil)
      ;; And _now_ these have their standard values.
      (expect auto-dark-dark-theme :to-be 'tsdh-dark)
      (expect auto-dark-light-theme :to-be 'tsdh-light))

    (it "should have enabled the correct themes"
      (expect custom-enabled-themes :to-be-in '((tsdh-dark) (tsdh-light))))))

;;; de-facto-old.el ends here
