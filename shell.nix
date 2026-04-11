# make it not suck on work on more than Darwin.
with import <nixpkgs> {};
let hs = haskell.packages.ghc9103.ghcWithPackages
    (p: with p; [cabal-install haskell-language-server]);
    # thisuhd = uhd.override {boost = boost183;};
    thisuhd = uhd;
in mkShell
{
    packages = [ hs
                 fftw
                 thisuhd.out
                 thisuhd.dev
                 haskellPackages.profiteur
                 haskellPackages.eventlog2html
                 typos
               ];
    shellHook = ''
        export DYLD_LIBRARY_PATH=${fftw}/lib:${thisuhd.out}/lib
    '';
}
