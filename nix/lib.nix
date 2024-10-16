{emacsPackages}: let
  emacsPath = package: "${package}/share/emacs/site-lisp/elpa/${package.pname}-${package.version}";
in {
  ## We need to tell Eldev where to find its Emacs package.
  ELDEV_LOCAL = emacsPath emacsPackages.eldev;

  ## Ideally this could just
  ##     (setq eldev-external-package-dir "${deps}/share/emacs/site-lisp/elpa")
  ## but Eldev wants to write to that directory, even if there's nothing to
  ## download.
  setUpLocalDependencies = deps: ''
    {
      echo
      echo "(mapcar 'eldev-use-local-dependency"
      echo "        (directory-files"
      echo "         \"${deps}/share/emacs/site-lisp/elpa\""
      echo "         t"
      echo "         directory-files-no-dot-files-regexp))"
    } >> Eldev
  '';
}
