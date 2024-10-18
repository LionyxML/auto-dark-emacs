;;; init.el --- An example configuration -*- lexical-binding: t; -*-

;; We recommend customizing variables before enabling modes, to avoid
;; “flickering” of values being set to their defaults initially, and then
;; updated. `custom-set-variables' is very fast unless the associated package
;; has already been loaded, in which case it may execute arbitrary code. So,
;; doing this first ensures that you get to the rest of your initialization
;; quickly, which can then be done in an order that matters to you.
(custom-set-variables
 '(auto-dark-themes '((tsdh-dark) (tsdh-light))))

;; The earlier you enable modes that affect the UI, the better, as these can
;; also cause flickering from the system defaults to the package configuration.
;; Unfortunately, this can’t generally be done in early-init, which would
;; otherwise allow the initial display of the frame to use the UI configuration.
(auto-dark-mode 1)

;; Non-UI initialization should go here.

;; `use-package' is also a good way to configure Auto-Dark. It’s built-in on
;; Emacs 29+, but must be installed on earlier versions.

;;; init.el ends here
