{ cabal, cmdargs, hslogger, parallelIo, regexPosix, shelly
, systemFileio, systemFilepath, text, transformers
}:

cabal.mkDerivation (self: {
  pname = "git-all";
  version = "1.5.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    cmdargs hslogger parallelIo regexPosix shelly systemFileio
    systemFilepath text transformers
  ];
  meta = {
    homepage = "https://github.com/jwiegley/git-all";
    description = "Determine which Git repositories need actions to be taken";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
