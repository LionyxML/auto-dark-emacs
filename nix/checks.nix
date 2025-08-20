{
  checkedDrv,
  emacsPackages,
  emacsWithPackages,
  src,
  stdenv,
}: let
  lib = import ./lib.nix {inherit emacsPackages;};
in {
  doctor = checkedDrv (stdenv.mkDerivation {
    inherit src;
    inherit (lib) ELDEV_LOCAL;

    name = "eldev doctor";

    nativeBuildInputs = [
      (emacsWithPackages (e: [e.elisp-lint]))
      # Emacs-lisp build tool, https://doublep.github.io/eldev/
      emacsPackages.eldev
    ];

    buildPhase = ''
      runHook preBuild
      ## TODO: Currently needed to make a temp file in
      ##      `eldev--create-internal-pseudoarchive-descriptor`.
      HOME="$(mktemp --directory --tmpdir fake-home.XXXXXX)"
      mkdir -p "$HOME/.cache/eldev"
      ## NB: `EMACSNATIVELOADPATH` is needed by `elisp-lin
      EMACSNATIVELOADPATH= eldev doctor
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p "$out"
      runHook postInstall
    '';
  });

  lint = let
    emacs = emacsWithPackages (e: [
      e.elisp-lint
      e.package-lint
      e.relint
    ]);
  in
    checkedDrv (stdenv.mkDerivation {
      inherit src;
      inherit (lib) ELDEV_LOCAL;

      name = "eldev lint";

      nativeBuildInputs = [
        emacs
        emacsPackages.eldev
      ];

      postPatch = lib.setUpLocalDependencies emacs.deps;

      buildPhase = ''
        runHook preBuild

        ## Need `--external` here so that we don’t try to download any
        ## package archives (which would break the sandbox).
        ## NB: `EMACSNATIVELOADPATH` is needed by `elisp-lint`.
        ## TODO: Currently need `HOME` to make a temp file in
        ##      `eldev--create-internal-pseudoarchive-descriptor`.
        EMACSNATIVELOADPATH= \
          HOME="$(mktemp --directory --tmpdir fake-home.XXXXXX)" \
          eldev --external lint --required

        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall
        mkdir -p "$out"
        runHook preInstall
      '';
    });
}
