{ mkDerivation, base, cmdargs, hslogger, parallel-io, regex-posix
, shelly, stdenv, system-fileio, system-filepath, text
, transformers, unix
}:
mkDerivation {
  pname = "git-all";
  version = "1.5.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base cmdargs hslogger parallel-io regex-posix shelly system-fileio
    system-filepath text transformers unix
  ];
  homepage = "https://github.com/jwiegley/git-all";
  description = "Determine which Git repositories need actions to be taken";
  license = stdenv.lib.licenses.bsd3;
}
