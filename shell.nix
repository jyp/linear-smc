{ nixpkgs ? import <nixpkgs> {} }:
let nixpkgs_source = fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-21.05.tar.gz;
    myNix = import nixpkgs_source {};
in
with myNix.pkgs; 
let hp9 = haskell.packages.ghc901; # needs latest hackage as well
    hp = hp9;
    ghc = hp.ghcWithPackages (ps: with ps; ([ cabal-install ]));
in pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ ghc ]; # git does not work easily here.
  shellHook = ''
 export LANG=en_US.UTF-8
 export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
 eval $(egrep ^export ${ghc}/bin/ghc)
'';
}

