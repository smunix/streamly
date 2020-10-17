# To include benchmark deps use:
# nix-shell --argstr c2nix "--benchmark"
#
# Use cabal flags as needed, e.g. to include examples:
# nix-shell --argstr c2nix "--flag examples"
#
# To build the chart executable for running bench.sh use:
# nix-shell --argstr c2nix "--flag dev" --run "cabal build chart --flag dev"
{
  nixpkgs ?
    import (builtins.fetchTarball https://github.com/composewell/nixpkgs/archive/01dd2b4e738.tar.gz)
        # fusion-plugin is marked a broken
        { config.allowBroken = true;}
, compiler ? "default"
, c2nix ? "" # cabal2nix CLI options
}:
let haskellPackages =
        if compiler == "default"
        then nixpkgs.haskellPackages
        else nixpkgs.haskell.packages.${compiler};

    # we can possibly avoid adding our package to HaskellPackages like
    # in the case of nix-shell for a single package?
    mkPackage = self: pkg: path: opts: inShell:
                let orig = self.callCabal2nixWithOptions pkg path opts {};
                 in if inShell
                    # Avoid copying the source directory to nix store by using
                    # src = null.
                    then orig.overrideAttrs (oldAttrs: { src = null; })
                    else orig;

    mkHaskellPackages = inShell:
        haskellPackages.override {
            packageSetConfig = self: super: {
                # This disables checks on all the packages.

                # Adding "doBenchmark = true" causes infinite recursion,
                # probably because of "super.mkDerivation". "mkDerivation" might
                # itself have benchmarks. We can probably directly call
                # "mkDerivation" and try. Need to search where it is defined.

                # Strangely enough mapping over haskellPackages and setting
                # "doCheck = false" where available, does not work. Probably
                # because of evaluation semantics.
                mkDerivation = drv: super.mkDerivation (drv // {
                    doCheck = false;
                });
            };
            overrides = self: super:
                with nixpkgs.haskell.lib;
                {
                    streamly = mkPackage self "streamly" ./. c2nix inShell;
                    streamly-benchmarks =
                        mkPackage self "streamly-benchmarks"
                            ./benchmark c2nix inShell;
                    QuickCheck = self.QuickCheck_2_14;
                    selective =
                        super.selective.overrideAttrs (oldAttrs:
                          { configureFlags =
                              oldAttrs.configureFlags ++ ["--disable-tests"];
                          });
                };
        };

    drv = mkHaskellPackages true;

    shell = drv.shellFor {
        packages = p:
          [ p.streamly
            p.streamly-benchmarks
          ];
        # some dependencies of hoogle fail to build with quickcheck-2.14
        # We should use hoogle as external tool instead of building it here
        # withHoogle = true;
        doBenchmark = true;
        # On macOs cabal2nix does not seem to generate a Cocoa
        # framework dependency.
        buildInputs =
            if builtins.currentSystem == "x86_64-darwin"
            then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
            else [];
        shellHook = ''
          if test -n "$PS_SHELL"
          then
            export PS1="$PS_SHELL$bldred(nix)$txtrst "
          fi
        '';
    };
in if nixpkgs.lib.inNixShell
   then shell
   else mkHaskellPackages false
