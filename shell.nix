# make it not suck on work on more than Darwin.
with import <nixpkgs> {};
let hs = haskell.packages.ghc9122.ghcWithPackages
    (p: with p; [cabal-install haskell-language-server]);
    thisuhd = uhd.override {boost = boost183;};
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
