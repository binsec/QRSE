let sources = import ./sources.nix {}; in
import sources.nixpkgs
    {
        overlays =
            [
                (
                    final: prev: 
                        {
                            popcon = (import (sources.popcon.outPath + "/nix/pkgs.nix")).popcon;
                            dunePlugins = prev.callPackage ./dunePlugins.nix {};
                            inherit (import ./binsec/pkgs.nix) binsec;
                            qrse = prev.callPackage ./qrse.nix {};
                            binsec_with_plugins = final.dunePlugins final.binsec [final.qrse];
                            final = final.callPackage ./final.nix {};
                        }
                )
            ];
    }
