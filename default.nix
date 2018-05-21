{ compiler    ? "ghc822" # "ghc842" also works
, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "95b1827682dc30ff1ccffb4f46c197289cea3e1c"
, sha256      ? "0v5s2918a04h6h1m18pzp36l5f41rhkipwqgysamsz7h0q4zwhwz"
, pkgs        ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let
  haskellPackages' = pkgs.haskell.packages.${compiler};

  haskellPackages = pkgs.lib.fix (this: haskellPackages'.override {
    overrides = with pkgs.haskell.lib; self: super: {
      enclosed-exceptions =
        if compiler == "ghc842"
        then dontCheck super.enclosed-exceptions
        else super.enclosed-exceptions;
    };
  });

in haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    enableLibraryProfiling    = doProfiling;
    enableExecutableProfiling = doProfiling;
    inherit doBenchmark;
  });

  inherit returnShellEnv;
}
