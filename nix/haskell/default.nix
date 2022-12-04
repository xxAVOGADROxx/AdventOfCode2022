{pkgs ? import ../nixpkgs {} }:

let
  inherit (pkgs.haskell) ghcVersion;

  hsPkgs = pkgs.haskell.packages.${ghcVersion};

  pkgDrv = hsPkgs.callCabal2nix "adventoofcode2022" ../.. {};
  haskellDeps = pkgDrv.getBuildInputs.haskellBuildInputs;
  ghc = hsPkgs.ghcWithHoogle (_: haskellDeps);

in
{
  inherit ghc;
  inherit (hsPkgs) cabal-install ghcide hlint ghcid stylish-haskell;
}
