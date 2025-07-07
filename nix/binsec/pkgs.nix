let 
    sources = import ../sources.nix {};
    overlay =
        self: super:
            {
                unisim_archisec = super.callPackage ./unisim_archisec.nix {};
                bitwuzla-cxx = super.callPackage ./bitwuzla.nix {};
                binsec = (super.callPackage ./binsec.nix {}).overrideAttrs(prev: {doCheck = false;});
            };
in
import sources.nixpkgs {overlays = [overlay];}
