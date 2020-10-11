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
            overrides = self: super:
                with nixpkgs.haskell.lib;
                {
                    streamly = mkPackage self "streamly" ./. c2nix inShell;
                    streamly-benchmarks =
                        mkPackage self "streamly-benchmarks"
                            ./benchmark c2nix inShell;
                    QuickCheck = self.QuickCheck_2_14;

                    # We need the ability to disable doCheck on all
                    # those packages that are being built locally and
                    # not fetched from the cache. Running tests could do
                    # nasty things to the machine e.g. some tests even
                    # listen for incoming connections on the network.
                    hspec-core =
                        super.hspec-core.overrideAttrs
                            (oldAttrs: {doCheck = false;});
                    selective =
                        super.selective.overrideAttrs (oldAttrs:
                          { doCheck = false;
                            configureFlags =
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
