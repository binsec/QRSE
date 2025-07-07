{stdenv, ocaml-ng, binsec, nix-gitignore}:
    let ocaml = ocaml-ng.ocamlPackages; in
    ocaml.buildDunePackage
        {
            pname = "qrse";
            version = "2.0";
            duneVersion = "3";
            src = nix-gitignore.gitignoreSource ["nix" "test" "*.md" "*.nix"] ./..;

            propagatedBuildInputs = [binsec];
        }
