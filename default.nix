((import ./reflex) { }).project ({ pkgs, ... }: {
    withHoogle = false;
    packages = {
        bulmex = ./bulmex;
    };
    overrides =
    let 
    noHpack = lib: (pkgs.haskell.lib.overrideCabal lib (drv: {
        # prevent hpack from being build for package.yaml on ghcjs
        libraryToolDepends = [ ]; 
        preConfigure = "";
        prePatch = "";
      }));
      in self: super: {
      reflex-dom-helpers = self.callPackage ./packages/reflex-dom-helpers.nix { };
      generic-lens = pkgs.haskell.lib.dontCheck (super.generic-lens);
      bulmex = noHpack super.bulmex;
    };

    shells = {
        ghc = ["bulmex"];
        ghcjs = ["bulmex" ];
    };
})
