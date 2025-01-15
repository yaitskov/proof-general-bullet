# https://nixos.wiki/wiki/Emacs
{ pkgs ? import <nixpkgs> {}
}:
with pkgs ;
let
  e = pkgs.emacs30;
in
e.pkgs.trivialBuild {
  pname = "proof-general-bullen";
  version = "0.0.1";

  src = ./.;

  packageRequires = [ e.pkgs.proof-general ];

  buildPhase = ''
    runHook preBuild

    echo BUILDING
    emacs -l package -f package-initialize -L lisp --batch -f batch-byte-compile lisp/*.el

    pushd test
    make clean
    make
    popd

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    LISPDIR=$out/share/emacs/site-lisp
    pushd lisp
    install -d $LISPDIR
    install *.el *.elc $LISPDIR
    popd

    runHook postInstall
  '';

  meta = {
    description = "...";
    license = lib.licenses.gpl3;
    platforms = lib.platforms.all;
  };
}
