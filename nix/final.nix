{stdenv, lib, binsec_with_plugins, popcon, makeWrapper}:
    stdenv.mkDerivation
        {
            pname = "binsec-final";
            version = binsec_with_plugins.version;
            src = binsec_with_plugins;
            nativeBuildInputs = [makeWrapper];
            propagatedBuildInputs = 
                [
                    binsec_with_plugins
                    popcon
                ];
            installPhase =
                ''
                    install -Dt $out/bin ${binsec_with_plugins}/bin/*
                    wrapProgram $out/bin/binsec --prefix PATH : ${lib.makeBinPath [popcon]}
                '';
        }
