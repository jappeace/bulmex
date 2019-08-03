let reflex-platform = builtins.fetchGit {
      url = https://github.com/reflex-frp/reflex-platform.git;
      ref = "develop";
      rev = "716879f16d53c93766e7ed9af17416fccb2edfe1";
    };
in (import reflex-platform { }).project ({ pkgs, ... }: {
    packages = {
        bulmex = ./bulmex;
    };
    overrides = self: super: {
      reflex-dom-helpers = self.callPackage ./packages/reflex-dom-helpers.nix { };
    };

    shells = {
        ghc = ["bulmex"];
        ghcjs = ["bulmex" ];
    };
})
