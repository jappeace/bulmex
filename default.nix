let reflex-platform = builtins.fetchGit {
      url = https://github.com/reflex-frp/reflex-platform.git;
      ref = "develop";
      rev = "d1ec7642e83ee48de3fd7cd5a3889d6a9585206f";
    };
in (import reflex-platform { }).project ({ pkgs, ... }: {
    withHoogle = false;
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
