;;; basic.el --- Tests that work in any environment -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'auto-dark)
(require 'buttercup)

;; Track whether `auto-dark-mode' was enabled before we started the tests.
(defvar auto-dark-tests--original-state auto-dark-mode)
(defvar auto-dark-tests--original-enabled-themes custom-enabled-themes)

(custom-set-variables
 ;; TODO: This is not a valid value, but it stops Auto-Dark from trying to set
 ;;       the detection method. See #73.
 '(auto-dark-detection-method 'manual)
 '(custom-enabled-themes ())
 '(frame-background-mode 'light))

;; Ensure `auto-dark-mode' is disabled for this part of the test.
(when auto-dark-tests--original-state
  (auto-dark-mode -1))

(describe "when disabled"
  (it "shouldn’t affect enabled themes"
    (expect custom-enabled-themes :to-be nil)
    (custom-set-variables
     '(auto-dark-themes '((tango-dark) (tango))))
    (expect auto-dark-themes :to-equal '((tango-dark) (tango)))
    (expect custom-enabled-themes :to-be nil)))

(describe "when enabled"
  (it "should immediately update themes"
    (expect auto-dark-themes :to-equal '((tango-dark) (tango)))
    (auto-dark-mode 1)
    (expect auto-dark-themes :to-equal '((tango-dark) (tango)))
    (expect custom-enabled-themes :to-equal '(tango)))
  (it "should reflect changes to themes immediately"
    (custom-set-variables
     '(auto-dark-themes '((wombat) (leuven))))
    (expect auto-dark-themes :to-equal '((wombat) (leuven)))
    (expect custom-enabled-themes :to-equal '(leuven))))

;; Restore the original state
(unless auto-dark-tests--original-state
  (auto-dark-mode -1))

(custom-set-variables
 '(custom-enabled-themes auto-dark-tests--original-enabled-themes))

;;; basic.el ends here
