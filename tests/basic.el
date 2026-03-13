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

(describe "toggling"
  (before-each
    (auto-dark-toggle-appearance))
  (it "should invert the current appearance"
    (expect custom-enabled-themes :to-equal '(wombat)))
  (it "should invert the current appearance again"
    (expect custom-enabled-themes :to-equal '(leuven)))
  (it "should invert the current appearance and again"
    (expect custom-enabled-themes :to-equal '(wombat))))

;; Test for themes that are declared but not fully loaded (issue #96).
;; modus-themes v5 eagerly calls `custom-declare-theme' for all its themes
;; when `(require 'modus-themes)' runs, making `custom-theme-p' return t
;; even though no face specs have been computed (`theme-settings' is nil).
;; auto-dark must detect this and use `load-theme' instead of `enable-theme'.

(describe "declared-but-not-loaded themes (issue #96)"
  :var (test-dark-theme test-light-theme)
  (before-all
    ;; Pick two built-in themes that haven't been loaded yet in this session.
    ;; We'll artificially "declare" the dark one (simulating what modus-themes
    ;; does at require-time) so it's known but has no face specs.
    (setq test-dark-theme 'tsdh-dark
          test-light-theme 'tsdh-light)
    ;; Make sure our test themes are in a clean state.
    (mapc #'disable-theme (list test-dark-theme test-light-theme))
    ;; Remove any prior theme-settings so we start fresh.
    (put test-dark-theme 'theme-settings nil)
    ;; Simulate modus-themes: declare the theme without loading it.
    ;; This adds it to `custom-known-themes' (so `custom-theme-p' returns t)
    ;; but does NOT populate `theme-settings' (no face specs).
    (unless (custom-theme-p test-dark-theme)
      (custom-declare-theme test-dark-theme
                            (custom-make-theme-feature test-dark-theme)))
    ;; Sanity check: the theme is "known" but has no settings.
    (expect (custom-theme-p test-dark-theme) :to-be-truthy)
    (expect (get test-dark-theme 'theme-settings) :to-be nil))

  (it "pre-load should load a declared-but-not-loaded theme"
    ;; The :set handler must not skip pre-loading just because the theme
    ;; is declared.
    (custom-set-variables
     `(auto-dark-themes '((,test-dark-theme) (,test-light-theme))))
    ;; After :set runs the pre-load, the theme should have settings.
    (expect (get test-dark-theme 'theme-settings) :to-be-truthy))

  (it "enable-themes should activate the theme correctly"
    ;; The current mode is light (from earlier tests), so toggle to dark.
    (auto-dark-toggle-appearance)
    (expect custom-enabled-themes :to-equal (list test-dark-theme)))

  (after-all
    ;; Clean up: switch back to a known state.
    (auto-dark-toggle-appearance)
    (mapc #'disable-theme (list test-dark-theme test-light-theme))
    (custom-set-variables
     '(auto-dark-themes '((wombat) (leuven))))))

;; Restore the original state
(unless auto-dark-tests--original-state
  (auto-dark-mode -1))

(custom-set-variables
 '(custom-enabled-themes auto-dark-tests--original-enabled-themes))

;;; basic.el ends here
