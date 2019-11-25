((import ./reflex) { }).project ({ pkgs, ... }: {
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
