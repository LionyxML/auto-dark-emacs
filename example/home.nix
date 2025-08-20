{lib, ...}: {
  programs.emacs = {
    enable = true;
    extraConfig = lib.readFile ./init.el;
    extraPackages = epkgs: [epkgs.auto-dark];
  };
}
