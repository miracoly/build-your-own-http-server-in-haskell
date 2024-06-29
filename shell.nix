let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/89c49874fb15f4124bf71ca5f42a04f2ee5825fd.tar.gz") {};
  ghcVersion = "ghc946";  # Corresponds to GHC 9.4.6
  ghcWithPackages = pkgs.haskell.packages.${ghcVersion}.ghcWithPackages (pkgs: with pkgs; [
    haskell-language-server
    hoogle
    # Add other Haskell dependencies here if needed
  ]);
in
pkgs.mkShell {
  buildInputs = [
    ghcWithPackages
    pkgs.stack
    pkgs.curlFull.dev
    pkgs.oha
    pkgs.zlib.dev
  ];

  shellHook = ''
    echo "Nix shell with GHC ${ghcVersion} and Haskell Language Server"
  '';
}
