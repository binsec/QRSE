{stdenv, lib, ocaml-ng, ocaml, makeWrapper, dune_3}:
    base: plugins:
        let
            add_build =
                d:
                    d.overrideAttrs
                        (
                            final: prev:
                                {
                                    outputs = (if prev ? outputs then prev.outputs else ["out"]) ++ ["build"];

                                    postPostInstall =
                                        ''
                                            mkdir -p $build
                                            cp -r _build $build
                                            cp dune-project $build
                                            echo ${prev.pname} > $build/name
                                        '';

                                    preFixupPhases = (if prev ? preFixupPhases then prev.preFixupPhases else []) ++ ["postPostInstall"];
                                }
                        )
            ;
            base_ = add_build base;
            plugins_ = lib.lists.forEach plugins add_build;
        in
        stdenv.mkDerivation
            {
                pname = base_.pname;
                version = base_.version;
                src = base_.src;
                dontUnpack = true;
                nativeBuildInputs =
                    [
                        makeWrapper
                        dune_3
                        ocaml
                    ];
                propagatedBuildInputs = lib.lists.subtractLists ([base] ++ plugins) (lib.lists.foldl (res: e: res ++ e.propagatedBuildInputs) base.propagatedBuildInputs plugins);
                installPhase =
                    ''
                        rm -rf tmp
                        export OCAMLPATH="$out/lib/ocaml/${ocaml.version}/site-lib:$OCAMLPATH"
                        for pkg in ${base_.build} ${lib.concatStrings (lib.lists.forEach plugins_ (p: "${p.build} "))}; do cp -r $pkg tmp; chmod -R +w tmp; cd tmp; dune install --prefix $out --libdir $OCAMLFIND_DESTDIR $(<name); cd ..; rm -rf tmp; done
                        
                        for bin in $out/bin/*; do wrapProgram $bin --prefix OCAMLPATH : $out/lib/ocaml/${ocaml.version}/site-lib || true; done
                    '';
            }
