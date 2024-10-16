;;; customize-first.el --- Customize vars before enabling -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests the recommended flow of customizing variables before enabling
;; Auto-Dark.

;;; Code:

(require 'auto-dark-initialize)
(require 'buttercup)

(describe "package initialization with variables pre-set"
  (before-all
    (auto-dark-initialize-after-early-init '(auto-dark-autoloads))
    (custom-set-variables
     '(auto-dark-themes '((tsdh-dark) (tsdh-light)))))
  (describe "before enabling"
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

  (describe "after enabling"
    (before-all
      (auto-dark-mode 1))
    (it "themes should be enabled"
      (expect custom-enabled-themes :to-be-in '((tsdh-dark) (tsdh-light))))
    (it "should have bound Auto-Dark variables"
      (expect auto-dark-allow-osascript :to-be nil)
      (expect auto-dark-allow-powershell :to-be nil)
      (expect (boundp 'auto-dark-detection-method) :to-be-truthy)
      (expect auto-dark-polling-interval-seconds :to-be 5)
      (expect auto-dark-themes :to-equal '((tsdh-dark) (tsdh-light)))
      ;; These two are handled specially – they aren’t set to their defaults
      ;; until after initialization.
      (expect (boundp 'auto-dark-dark-theme) :to-be nil)
      (expect (boundp 'auto-dark-light-theme) :to-be nil)))

  (describe "after init"
    (before-all
      (auto-dark-initialize-finish))
    (it "themes should be enabled"
      (expect custom-enabled-themes :to-be-in '((tsdh-dark) (tsdh-light))))
    (it "Old Auto-Dark variables should now be set"
      (expect auto-dark-allow-osascript :to-be nil)
      (expect auto-dark-allow-powershell :to-be nil)
      (expect (boundp 'auto-dark-detection-method) :to-be-truthy)
      (expect auto-dark-polling-interval-seconds :to-be 5)
      (expect auto-dark-themes :to-equal '((tsdh-dark) (tsdh-light)))
      (expect auto-dark-dark-theme :to-be 'wombat)
      (expect auto-dark-light-theme :to-be 'leuven))))

;;; customize-first.el ends here
