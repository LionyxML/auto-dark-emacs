{
  checkedDrv,
  emacsPackages,
  src,
}: let
  lib = import ../lib.nix {inherit emacsPackages;};

  ## Read version in format: ;; Version: x.y(.z)?
  readVersion = fp:
    builtins.elemAt
    (builtins.match
      ".*(;; Version: ([[:digit:]]+\.[[:digit:]]+(\.[[:digit:]]+)?)).*"
      (builtins.readFile fp))
    1;

  emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
    epkgs.buttercup
    epkgs.elisp-lint
    epkgs.use-package # TODO: Conditionalize on Emacs <29?
  ]);
in
  checkedDrv (emacsPackages.trivialBuild {
    inherit src;
    inherit (lib) ELDEV_LOCAL;

    pname = "auto-dark";

    version = readVersion ../../auto-dark.el;

    nativeBuildInputs = [
      emacsWithPackages
      # Emacs-lisp build tool, https://doublep.github.io/eldev/
      emacsPackages.eldev
    ];

    postPatch = lib.setUpLocalDependencies emacsWithPackages.deps;

    doCheck = true;

    checkPhase = ''
      runHook preCheck

      ## TODO: Currently needed to make a temp file in
      ##      `eldev--create-internal-pseudoarchive-descriptor`.
      HOME="$(mktemp --directory --tmpdir fake-home.XXXXXX)"

      ## Need `--external` here so that we don’t try to download any
      ## package archives (which would break the sandbox).
      eldev --external test --test-type=main

      runHook postCheck
    '';

    doInstallCheck = true;

    installCheckPhase = ''
      runHook preInstallCheck

      init_tests=($(IFS=$'\n' find ./tests/initialization -type f -name '*.el'))
      # This tries to make sure we don’t have a bug that skips the tests.
      if (( ''${#init_tests[@]} == 0 )); then
        echo "didn’t find initialization tests" >2
        exit 1
      else
        for file in "''${init_tests[@]}"; do
          echo "testing $file"
          eldev --external test --test-type=integration -f "$file"
        done
      fi

      ## TODO: Currently needed to make a temp file in
      ##      `eldev--create-internal-pseudoarchive-descriptor`.
      HOME="$(mktemp --directory --tmpdir fake-home.XXXXXX)"

      ## Need `--external` here so that we don’t try to download any
      ## package archives (which would break the sandbox).
      eldev --external --packaged test --test-type=main

      runHook postInstallCheck
    '';
  })
