let 
    sources = import ../nix/sources.nix {};
    pkgs = import sources.nixpkgs {};
in
pkgs.mkShell
    {
        packages = [pkgs.headache];
    }

