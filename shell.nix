let pkgs = import ./nix/pkgs.nix; in
pkgs.mkShell
    {
        inputsFrom = [pkgs.final];
    }
