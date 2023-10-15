{ compiler    ? "ghc882"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "24c765c744ba856700db92ab94ef9c695d73f95f"
, sha256      ? "0ak482k4jsnnmipmc038fla5ywr9h01xs91sjkx35wkkxcs2lc23"
, pkgs        ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let haskellPackages = pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
    enclosed-exceptions =
      if compiler == "ghc843"
      then dontCheck super.enclosed-exceptions
      else super.enclosed-exceptions;
  };

  source-overrides = {
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    inherit doBenchmark;
  });

  inherit returnShellEnv;
}
