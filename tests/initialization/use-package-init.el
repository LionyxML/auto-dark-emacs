;;; use-package-init.el --- Use-package with :init -*- lexical-binding: t; -*-

;;; Commentary:

;; This tests a common `use-package' style of initialization.

;;; Code:

(require 'auto-dark-initialize)
(require 'buttercup)

;; To silence “reference to free variable” warnings
(defvar auto-dark-allow-osascript)
(defvar auto-dark-allow-powershell)
(defvar auto-dark-polling-interval-seconds)
(defvar auto-dark-themes)

(describe "‘use-package’ initialization with :init"
  (before-all
    (auto-dark-initialize-after-early-init '(auto-dark-autoloads)))

  (describe "after ‘package-initialize’ is called"
    (it "should not have enabled the themes yet"
      (expect custom-enabled-themes :to-be ()))
    (it "should not have bound Auto-Dark variables"
      (expect (boundp 'auto-dark-allow-osascript) :to-be nil)
      (expect (boundp 'auto-dark-allow-powershell) :to-be nil)
      (expect (boundp 'auto-dark-detection-method) :to-be nil)
      (expect (boundp 'auto-dark-polling-interval-seconds) :to-be nil)
      (expect (boundp 'auto-dark-themes) :to-be nil)))

  (describe "after ‘user-init-file’ is loaded"
    (before-all
      (use-package auto-dark
        :custom (auto-dark-themes '((tsdh-dark) (tsdh-light)))
        :config (auto-dark-mode)))

    (it "should have configured Auto-Dark"
      (expect auto-dark-allow-osascript :to-be nil)
      (expect auto-dark-allow-powershell :to-be nil)
      (expect (boundp 'auto-dark-detection-method) :to-be-truthy)
      (expect auto-dark-polling-interval-seconds :to-be 5)
      (expect auto-dark-themes :to-equal '((tsdh-dark) (tsdh-light))))
    (it "should have enabled the themes"
      (expect custom-enabled-themes :to-be-in '((tsdh-dark) (tsdh-light))))))

;;; use-package-init.el ends here
