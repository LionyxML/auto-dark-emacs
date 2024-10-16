;;; early-init.el --- Customize in early-init -*- lexical-binding: t; -*-

;;; Commentary:

;; This is more of an edge case, but it customizes the variables in early init,
;; and then enables the package in regular init.

;;; Code:

(require 'auto-dark-initialize)
(require 'buttercup)

;; To silence “reference to free variable” warnings
(defvar auto-dark-themes)

(describe "customization in early init"
  (before-all
    (auto-dark-initialize-start)
    (custom-set-variables
     '(auto-dark-themes '((tsdh-dark) (tsdh-light)))))

  (describe "in early init"
    (it "should not have enabled the themes yet"
      (expect custom-enabled-themes :to-be ())
      (expect (boundp 'auto-dark-themes) :to-be nil))
    (it "should not have bound Auto-Dark variables"
      (expect (boundp 'auto-dark-allow-osascript) :to-be nil)
      (expect (boundp 'auto-dark-allow-powershell) :to-be nil)
      (expect (boundp 'auto-dark-dark-theme) :to-be nil)
      (expect (boundp 'auto-dark-detection-method) :to-be nil)
      (expect (boundp 'auto-dark-light-theme) :to-be nil)
      (expect (boundp 'auto-dark-polling-interval-seconds) :to-be nil)
      (expect (boundp 'auto-dark-themes) :to-be nil)))

  (describe "after early init"
    (before-all
      (auto-dark-initialize-after-early-init '(auto-dark-autoloads)))
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

  (describe "after package is loaded"
    (before-all
      (require 'auto-dark))
    (it "should not have enabled the themes yet"
      (expect custom-enabled-themes :to-be ()))
    (it "should have bound Auto-Dark variables"
      (expect auto-dark-allow-osascript :to-be nil)
      (expect auto-dark-allow-powershell :to-be nil)
      (expect (boundp 'auto-dark-detection-method) :to-be-truthy)
      (expect auto-dark-polling-interval-seconds :to-be 5)
      (expect auto-dark-themes :to-equal '((tsdh-dark) (tsdh-light)))
      (expect auto-dark-dark-theme :to-be 'wombat)
      (expect auto-dark-light-theme :to-be 'leuven)))

  ;; FIXME: Make sure this is always in a particular mode, or check for either set.
  (describe "after-enabling"
    (before-all
      (auto-dark-mode 1))
    (it "themes should be enabled"
      (expect custom-enabled-themes :to-be-in '((tsdh-dark) (tsdh-light))))
    (it "Auto-Dark variables should be unchanged"
      (expect auto-dark-allow-osascript :to-be nil)
      (expect auto-dark-allow-powershell :to-be nil)
      (expect (boundp 'auto-dark-detection-method) :to-be-truthy)
      (expect auto-dark-polling-interval-seconds :to-be 5)
      (expect auto-dark-themes :to-equal '((tsdh-dark) (tsdh-light)))
      (expect auto-dark-dark-theme :to-be 'wombat)
      (expect auto-dark-light-theme :to-be 'leuven))))

;;; early-init.el ends here
