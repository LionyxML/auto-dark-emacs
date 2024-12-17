;; TODO: We have to be careful what variables we set here. Some can cause the
;;       Eldev linters to not read the settings here. See emacs-eldev/eldev#83.
((nil
  ;; FIXME: This is just set to silence linter line-length warnings. It should
  ;;       be set to an intentional value, then the long-lines fixed.
  (fill-column . 136)
  (indent-tabs-mode . nil)
  (sentence-end-double-space . nil)))
